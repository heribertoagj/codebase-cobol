      ******************************************************************
      * NOME DA INC - GFCTWAJR                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - SAIDA    *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO CONJUNTO SERVICOS               - GFCT5169 *
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
       01  WAJR-SAIDA.
           05  WAJR-LL                 PIC S9(004) COMP.
           05  WAJR-ZZ                 PIC S9(004) COMP.
           05  WAJR-TRANSACAO          PIC  X(009).
           05  WAJR-FUNCAO             PIC  X(008).
           05  WAJR-DADOS-RST.
               10  WAJR-QTDE-OCOR      PIC  9(007).
               10  WAJR-QTDE-TOT-REG   PIC  9(009).
               10  WAJR-FUNC-BDSCO     PIC  X(007).
               10  WAJR-FILTRO.
                   15  WAJR-FLT-TIPO-HIST
                                       PIC  X(001).
      *---             T = TODOS
      *---             P = PEDIDOS
      *---             D = DEFERIDOS NAO VIGENTES
                   15  WAJR-FLT-CAGPTO-CTA
                                       PIC  9(003).
ST2507*            15  WAJR-FLT-CCGC-CPF
ST2507*                                PIC  9(009).
ST2507*                                PIC  9(005).
ST2507             15  WAJR-FLT-CCGC-CPF
ST2507                                 PIC  X(009).
ST2507             15  WAJR-FLT-CFLIAL-CGC
ST2507                                 PIC  X(004).
                   15  WAJR-FLT-CCTRL-CPF-CGC
                                       PIC  9(002).
                   15  WAJR-FLT-CDEPDC PIC  9(005).
                   15  WAJR-FLT-CPOSTO-SERVC
                                       PIC  9(005).
                   15  WAJR-FLT-CSGMTO-TARIF
                                       PIC  9(003).
                   15  WAJR-FLT-CMUN-IBGE
                                       PIC  9(007).
                   15  WAJR-FLT-CSGL-UF
                                       PIC  X(002).
                   15  WAJR-FLT-GPO-CTBIL
                                       PIC  9(003).
                   15  WAJR-FLT-SUB-GPO-CTBIL
               10  WAJR-PONTEIRO.
                   15  WAJR-PTR-CCJTO-SERVC
                                       PIC  9(003).
           05  WAJR-ERRO               PIC  9(001).
           05  WAJR-COD-MSG-ERRO       PIC  9(007).
           05  WAJR-COD-SQL-ERRO       PIC  9(003).
           05  WAJR-DESC-MSG-ERRO      PIC  X(079).
           05  WAJR-FIM                PIC  X(001).
           05  FILLER                  PIC  X(164).
           05  WAJR-DADOS              OCCURS 20.
               10  WAJR-COD-CONJ-SERVC PIC  9(003).
               10  WAJR-DESCR-CONJ-SERVC
                                       PIC  X(030).
