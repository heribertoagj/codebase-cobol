      *****************************************************************
      * NOME DA INC - GFCTWANV                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               TEXT DE RAZAO - FLEXIBILIZACAO AGRUPAMENTO      *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                   GFCT5298  *
      * DATA        - 06.07.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA          - PROCWORK               *
      *****************************************************************
      *
       01  WANV-ENTRADA.
           03  WANV-LL                      PIC S9(004) COMP.
           03  WANV-ZZ                      PIC S9(004) COMP.
           03  WANV-TRANSACAO               PIC  X(009).
           03  WANV-FUNCAO                  PIC  X(008).
           03  WANV-FUNC-BDSCO              PIC  X(007).
           03  WANV-FIM                     PIC  X(001).
           03  WANV-DADOS.
               05  WANV-GRUPO-CONTABIL      PIC  9(003).
               05  WANV-SUB-GRUPO-CONTABIL  PIC  9(003).
           03  WANV-FILLER                  PIC  X(065).
