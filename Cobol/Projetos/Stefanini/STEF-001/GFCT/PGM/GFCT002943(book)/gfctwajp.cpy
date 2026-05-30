      ******************************************************************
      * NOME DA INC - GFCTWAJP                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - SAIDA    *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA VIGENTES                                *
      *               COMBO CONJUNTO SERVICOS               - GFCT5166 *
      * TAMANHO     - 1010 - TOTAL                                     *
      *               0350 - FIXO                                      *
      *               0660 - VARIAVEL (33 X 20)                        *
      * DATA        - 24.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
       01  WAJP-SAIDA.
           05  WAJP-LL                 PIC S9(004) COMP.
           05  WAJP-ZZ                 PIC S9(004) COMP.
           05  WAJP-TRANSACAO          PIC  X(009).
           05  WAJP-FUNCAO             PIC  X(008).
           05  WAJP-DADOS-RST.
               10  WAJP-QTDE-OCOR      PIC  9(007).
               10  WAJP-QTDE-TOT-REG   PIC  9(009).
               10  WAJP-FUNC-BDSCO     PIC  X(007).
               10  WAJP-FILTRO.
                   15  WAJP-FLT-CAGPTO-CTA
                                       PIC  9(003).
ST2507*            15  WAJP-FLT-CCGC-CPF
ST2507*                                PIC  9(009).
ST2507*            15  WAJP-FLT-CFLIAL-CGC
ST2507*                                PIC  9(005).
ST2507             15  WAJP-FLT-CCGC-CPF
ST2507                                 PIC  X(009).
ST2507             15  WAJP-FLT-CFLIAL-CGC
                   15  WAJP-FLT-CCTRL-CPF-CGC
                                       PIC  9(002).
                   15  WAJP-FLT-CDEPDC
                                       PIC  9(005).
                   15  WAJP-FLT-CPOSTO-SERVC
                                       PIC  9(005).
                   15  WAJP-FLT-CSGMTO-TARIF
                                       PIC  9(003).
                   15  WAJP-FLT-CMUN-IBGE
                                       PIC  9(007).
                   15  WAJP-FLT-CSGL-UF
                                       PIC  X(002).
                   15  WAJP-FLT-GPO-CTBIL
                                       PIC  9(003).
                   15  WAJP-FLT-SUB-GPO-CTBIL
                                       PIC  9(003).
               10  WAJP-PONTEIRO.
                   15  WAJP-PTR-CCJTO-SERVC
                                       PIC  9(003).
           05  WAJP-COD-MSG-ERRO       PIC  9(007).
           05  WAJP-COD-SQL-ERRO       PIC  9(003).
           05  WAJP-DESC-MSG-ERRO      PIC  X(079).
           05  WAJP-FIM                PIC  X(001).
           05  FILLER                  PIC  X(165).
           05  WAJP-DADOS              OCCURS 20.
               10  WAJP-COD-CONJ-SERVC PIC  9(003).
               10  WAJP-DESCR-CONJ-SERVC
                                       PIC  X(030).
