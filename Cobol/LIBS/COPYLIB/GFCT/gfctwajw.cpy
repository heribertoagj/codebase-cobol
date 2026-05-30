      ******************************************************************
      * NOME DA INC - GFCTWAJW                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - ENTRADA  *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO UF FLEXIBILIZACAO VIGENTE       - GFCT5183 *
      * TAMANHO     - 100                                              *
      * DATA        - 27.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAJW-ENTRADA.
           05  WAJW-LL                 PIC S9(004) COMP.
           05  WAJW-ZZ                 PIC S9(004) COMP.
           05  WAJW-TRANSACAO          PIC  X(009).
           05  WAJW-FUNCAO             PIC  X(008).
           05  WAJW-DADOS-RST.
               10  WAJW-QTDE-OCOR      PIC  9(007).
               10  WAJW-QTDE-TOT-REG   PIC  9(009).
               10  WAJW-FUNC-BDSCO     PIC  X(007).
               10  WAJW-FILTRO.
                   15  WAJW-FLT-CAGPTO-CTA
                                       PIC  9(003).
               10  WAJW-PONTEIRO.
                   15  WAJW-PTR-CSGL-UF
                                       PIC  X(002).
           05  WAJW-FIM                PIC  X(001).
           05  FILLER                  PIC  X(050).
