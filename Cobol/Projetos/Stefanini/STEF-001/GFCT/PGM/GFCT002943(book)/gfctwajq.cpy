      ******************************************************************
      * NOME DA INC - GFCTWAJQ                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - ENTRADA  *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO CONJUNTO SERVICOS               - GFCT5169 *
      * TAMANHO     - 100                                              *
      * DATA        - 24.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAJQ-ENTRADA.
           05  WAJQ-LL                 PIC S9(004) COMP.
           05  WAJQ-ZZ                 PIC S9(004) COMP.
           05  WAJQ-TRANSACAO          PIC  X(009).
           05  WAJQ-FUNCAO             PIC  X(008).
           05  WAJQ-DADOS-RST.
               10  WAJQ-QTDE-OCOR      PIC  9(007).
               10  WAJQ-QTDE-TOT-REG   PIC  9(009).
               10  WAJQ-FUNC-BDSCO     PIC  X(007).
               10  WAJQ-FILTRO.
                   15  WAJQ-FLT-TIPO-HIST
                                       PIC  X(001).
      *---             T = TODOS
      *---             P = PEDIDOS
      *---             D = DEFERIDOS NAO VIGENTES
                   15  WAJQ-FLT-CAGPTO-CTA
                                       PIC  9(003).
ST2506*            15  WAJQ-FLT-CCGC-CPF
ST2506*                                PIC  9(009).
ST2506*            15  WAJQ-FLT-CFLIAL-CGC
ST2506*                                PIC  9(005).
ST2506             15  WAJQ-FLT-CCGC-CPF
ST2506                                 PIC  X(009).
ST2506             15  WAJQ-FLT-CFLIAL-CGC
ST2506                                 PIC  X(004).
                   15  WAJQ-FLT-CCTRL-CPF-CGC
                                       PIC  9(002).
                   15  WAJQ-FLT-CDEPDC PIC  9(005).
                   15  WAJQ-FLT-CPOSTO-SERVC
                                       PIC  9(005).
                   15  WAJQ-FLT-CSGMTO-TARIF
                                       PIC  9(003).
                   15  WAJQ-FLT-CMUN-IBGE
                                       PIC  9(007).
                   15  WAJQ-FLT-CSGL-UF
                                       PIC  X(002).
                   15  WAJQ-FLT-GPO-CTBIL
                                       PIC  9(003).
                   15  WAJQ-FLT-SUB-GPO-CTBIL
                                       PIC  9(003).
               10  WAJQ-PONTEIRO.
                   15  WAJQ-PTR-CCJTO-SERVC
                                       PIC  9(003).
           05  WAJQ-FIM                PIC  X(001).
           05  FILLER                  PIC  X(004).
