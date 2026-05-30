      *****************************************************************
      * NOME DA INC - GFCTWANW                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               TEXT DE RAZAO - FLEXIBILIZACAO AGRUPAMENTO      *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 0300                                 GFCT5298   *
      * DATA        - 06.07.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA         - PROCWORK                *
      *****************************************************************
      *
       01  WANW-SAIDA.
           03  WANW-LL                      PIC S9(004) COMP.
           03  WANW-ZZ                      PIC S9(004) COMP.
           03  WANW-TRANSACAO               PIC  X(009).
           03  WANW-FUNCAO                  PIC  X(008).
           03  WANW-FUNC-BDSCO              PIC  X(007).
           03  WANW-FIM                     PIC  X(001).
           03  WANW-ERRO                    PIC  9(001).
           03  WANW-COD-MSG-ERRO            PIC  9(007).
           03  WANW-COD-SQL-ERRO            PIC  9(003).
           03  WANW-DESC-MSG-ERRO           PIC  X(079).
           03  WANW-DADOS.
               05  WANW-GRUPO-CONTABIL      PIC  9(003).
               05  WANW-SUB-GRUPO-CONTABIL  PIC  9(003).
               05  WANW-DESC-CONTABIL       PIC  X(040).
           03  FILLER                       PIC  X(135).
