      ******************************************************************
      * NOME DA INC - GFCTWAJY                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - ENTRADA  *
      *               FLEXIBILIZACAO PARA AGRUPAMENTO                  *
      *               CONSULTA HISTORICOS                              *
      *               COMBO UF HISTORICO FLEXIBILIZACAO     - GFCT5207 *
      * TAMANHO     - 100                                              *
      * DATA        - 27.11.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAJY-ENTRADA.
           05  WAJY-LL                 PIC S9(004) COMP.
           05  WAJY-ZZ                 PIC S9(004) COMP.
           05  WAJY-TRANSACAO          PIC  X(009).
           05  WAJY-FUNCAO             PIC  X(008).
           05  WAJY-DADOS-RST.
               10  WAJY-QTDE-OCOR      PIC  9(007).
               10  WAJY-QTDE-TOT-REG   PIC  9(009).
               10  WAJY-FUNC-BDSCO     PIC  X(007).
               10  WAJY-FILTRO.
                   15  WAJY-FLT-TIPO-HIST
                                       PIC  X(001).
      *---             T = TODOS
      *---             P = PEDIDOS
      *---             D = DEFERIDOS NAO VIGENTES
                   15  WAJY-FLT-CAGPTO-CTA
                                       PIC  9(003).
               10  WAJY-PONTEIRO.
                   15  WAJY-PTR-CSGL-UF
                                       PIC  X(002).
           05  WAJY-FIM                PIC  X(001).
           05  FILLER                  PIC  X(049).

