      *****************************************************************
      * NOME DA INC - GFCTWANX                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               COMBO DE RAZAO - FLEXIBILIZACAO AGRUPAMENTO     *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                   GFCT5299  *
      * DATA        - 06.07.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA          - PROCWORK               *
      *****************************************************************
      *
       01  WANX-ENTRADA.
           03  WANX-LL                           PIC S9(004) COMP.
           03  WANX-ZZ                           PIC S9(004) COMP.
           03  WANX-TRANSACAO                    PIC  X(009).
           03  WANX-FUNCAO                       PIC  X(008).
           03  WANX-DADOS-RST.
               05  WANX-FILTRO-CAGPTO-CTA        PIC  9(003).
               05  WANX-FILTRO-GRP               PIC  9(003).
               05  WANX-FILTRO-SGRP              PIC  9(003).
               05  WANX-QTDE-OCOR                PIC  9(007).
               05  WANX-QTDE-TOT-REG             PIC  9(009).
               05  WANX-FUNC-BDSCO               PIC  X(007).
               05  WANX-PONTEIRO.
                   07 WANX-PONT-DESPR            PIC  9(005).
               05  WANX-FIM                      PIC  X(001).
           03  FILLER                            PIC  X(041).
