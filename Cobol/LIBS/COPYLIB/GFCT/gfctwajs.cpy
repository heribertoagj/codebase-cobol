      ******************************************************************
      * NOME DA INC - GFCTWAJS                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - ENTRADA  *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO SEGMENTO FLEXIBILIZACAO VIGENTE - GFCT5173 *
      * TAMANHO     - 100                                              *
      * DATA        - 27.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAJS-ENTRADA.
           05  WAJS-LL                 PIC S9(004) COMP.
           05  WAJS-ZZ                 PIC S9(004) COMP.
           05  WAJS-TRANSACAO          PIC  X(009).
           05  WAJS-FUNCAO             PIC  X(008).
           05  WAJS-DADOS-RST.
               10  WAJS-QTDE-OCOR      PIC  9(007).
               10  WAJS-QTDE-TOT-REG   PIC  9(009).
               10  WAJS-FUNC-BDSCO     PIC  X(007).
               10  WAJS-FILTRO.
                   15  WAJS-FLT-CAGPTO-CTA
                                       PIC  9(003).
               10  WAJS-PONTEIRO.
                   15  WAJS-PTR-CSGMTO-TARIF
                                       PIC  9(003).
           05  WAJS-FIM                PIC  X(001).
           05  FILLER                  PIC  X(049).
