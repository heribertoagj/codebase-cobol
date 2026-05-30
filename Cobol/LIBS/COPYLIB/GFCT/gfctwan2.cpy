      *****************************************************************
      * NOME DA INC - GFCTWAN2                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               COMBO RAZAO - FLEXIBILIZACAO                    *
      *               POR AGRUPAMENTO                                 *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                 GFCT5301   *
      * DATA        - 07.07.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA         - PROCWORK                *
      *****************************************************************
      *
       01  WAN2-SAIDA.
           05  WAN2-LL                           PIC S9(004) COMP.
           05  WAN2-ZZ                           PIC S9(004) COMP.
           05  WAN2-TRANSACAO                    PIC  X(009).
           05  WAN2-FUNCAO                       PIC  X(008).
           05  WAN2-DADOS-RST.
               07  WAN2-QTDE-OCOR                PIC  9(007).
               07  WAN2-QTDE-TOT-REG             PIC  9(009).
               07  WAN2-FUNC-BDSCO               PIC  X(007).
               07  WAN2-FILTRO.
                   09  WAN2-FLT-CAGPTO-CTA       PIC  9(003).
               07  WAN2-PONTEIRO.
                   09  WAN2-PONT-DESPR           PIC  9(005).
           05  WAN2-FIM                          PIC  X(001).
           05  WAN2-ERRO                         PIC  9(001).
           05  WAN2-COD-MSG-ERRO                 PIC  9(007).
           05  WAN2-COD-SQL-ERRO                 PIC  9(003).
           05  WAN2-DESC-MSG-ERRO                PIC  X(079).
           05  FILLER                            PIC  X(131).
           05  WAN2-DADOS.
               07  FILLER                        OCCURS 16.
                   09  WAN2-GRUPO-CTBIL          PIC  9(003).
                   09  WAN2-SUB-GRUPO-CTBIL      PIC  9(003).
                   09  WAN2-DESC-CTBIL           PIC  X(040).
