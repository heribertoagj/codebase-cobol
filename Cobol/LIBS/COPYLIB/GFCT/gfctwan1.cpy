      *****************************************************************
      * NOME DA INC - GFCTWAN1                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               COMBO DE RAZAO - FLEXIBILIZACAO AGRUPAMENTO     *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                   GFCT5301  *
      * DATA        - 07.07.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA          - PROCWORK               *
      *****************************************************************
      *
       01  WAN1-ENTRADA.
           03  WAN1-LL                           PIC S9(004) COMP.
           03  WAN1-ZZ                           PIC S9(004) COMP.
           03  WAN1-TRANSACAO                    PIC  X(009).
           03  WAN1-FUNCAO                       PIC  X(008).
           03  WAN1-DADOS-RST.
               05  WAN1-QTDE-OCOR                PIC  9(007).
               05  WAN1-QTDE-TOT-REG             PIC  9(009).
               05  WAN1-FUNC-BDSCO               PIC  X(007).
               05  WAN1-FILTRO.
                   07  WAN1-FLT-CAGPTO-CTA       PIC  9(003).
               05  WAN1-PONTEIRO.
                   07  WAN1-PONT-DESPR           PIC  9(005).
               05  WAN1-FIM                      PIC  X(001).
           03  FILLER                            PIC  X(047).
