      *****************************************************************
      * NOME DA INC - GFCTWANZ                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               COMBO DE RAZAO - FLEXIBILIZACAO AGRUPAMENTO     *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                   GFCT5300  *
      * DATA        - 06.07.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA          - PROCWORK               *
      *****************************************************************
      *
       01  WANZ-ENTRADA.
           03  WANZ-LL                           PIC S9(004) COMP.
           03  WANZ-ZZ                           PIC S9(004) COMP.
           03  WANZ-TRANSACAO                    PIC  X(009).
           03  WANZ-FUNCAO                       PIC  X(008).
           03  WANZ-DADOS-RST.
               05  WANZ-QTDE-OCOR                PIC  9(007).
               05  WANZ-QTDE-TOT-REG             PIC  9(009).
               05  WANZ-FUNC-BDSCO               PIC  X(007).
               05  WANZ-FILTRO.
                   07  WANZ-FLT-TIPO-HIST        PIC  X(001).
      *---             T = TODOS
      *---             P = PEDIDOS
      *---             D = DEFERIDOS NAO VIGENTES
                   07  WANZ-FLT-CAGPTO-CTA       PIC  9(003).
               05  WANZ-PONTEIRO.
                   07  WANZ-PONT-DESPR           PIC  9(005).
               05  WANZ-FIM                      PIC  X(001).
           03  FILLER                            PIC  X(046).
