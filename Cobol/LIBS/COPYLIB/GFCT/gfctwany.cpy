      *****************************************************************
      * NOME DA INC - GFCTWANY                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               COMBO RAZAO - FLEXIBILIZACAO                    *
      *               POR AG/CONTA                                    *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                 GFCT5299   *
      * DATA        - 16.02.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA         - PROCWORK                *
      *****************************************************************
      *
       01  WANY-SAIDA.
           03  WANY-LL                           PIC S9(004) COMP.
           03  WANY-ZZ                           PIC S9(004) COMP.
           03  WANY-TRANSACAO                    PIC  X(009).
           03  WANY-FUNCAO                       PIC  X(008).
           03  WANY-FILTRO-CAGPTO-CTA            PIC  9(003).
           03  WANY-FILTRO-GRP                   PIC  9(003).
           03  WANY-FILTRO-SGRP                  PIC  9(003).
           03  WANY-QTDE-OCOR                    PIC  9(007).
           03  WANY-QTDE-TOT-REG                 PIC  9(009).
           03  WANY-FUNC-BDSCO                   PIC  X(007).
           03  WANY-PONTEIRO.
               05  WANY-PONT-DESPR               PIC  9(005).
           03  WANY-FIM                          PIC  X(001).
           03  WANY-ERRO                         PIC  9(001).
           03  WANY-COD-MSG-ERRO                 PIC  9(007).
           03  WANY-COD-SQL-ERRO                 PIC  9(003).
           03  WANY-DESC-MSG-ERRO                PIC  X(079).
           03  FILLER                            PIC  X(125).
           03  WANY-DADOS.
               05  FILLER                      OCCURS 16.
                   07  WANY-GRUPO-CTBIL          PIC  9(003).
                   07  WANY-SUB-GRUPO-CTBIL      PIC  9(003).
                   07  WANY-DESC-CTBIL           PIC  X(040).
