      ******************************************************************
      * NOME DA INC - GFCTWAJU                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - ENTRADA  *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO SEGMENTO HISTORICO FLEXIBILIZ   - GFCT5179 *
      * TAMANHO     - 100                                              *
      * DATA        - 27.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAJU-ENTRADA.
           05  WAJU-LL                 PIC S9(004) COMP.
           05  WAJU-ZZ                 PIC S9(004) COMP.
           05  WAJU-TRANSACAO          PIC  X(009).
           05  WAJU-FUNCAO             PIC  X(008).
           05  WAJU-DADOS-RST.
               10  WAJU-QTDE-OCOR      PIC  9(007).
               10  WAJU-QTDE-TOT-REG   PIC  9(009).
               10  WAJU-FUNC-BDSCO     PIC  X(007).
               10  WAJU-FILTRO.
                   15  WAJU-FLT-TIPO-HIST
                                       PIC  X(001).
      *---             T = TODOS
      *---             P = PEDIDOS
      *---             D = DEFERIDOS NAO VIGENTES
                   15  WAJU-FLT-CAGPTO-CTA
                                       PIC  9(003).
               10  WAJU-PONTEIRO.
                   15  WAJU-PTR-CSGMTO-TARIF
                                       PIC  9(003).
           05  WAJU-FIM                PIC  X(001).
           05  FILLER                  PIC  X(048).
