      *****************************************************************
      * NOME DA INC - GFCTWAN0                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               COMBO RAZAO - FLEXIBILIZACAO                    *
      *               POR AG/CONTA                                    *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                 GFCT5300   *
      * DATA        - 07.07.2010                                      *
      * RESPONSAVEL - WELLEN FRANCA         - PROCWORK                *
      *****************************************************************
      *
       01  WAN0-SAIDA.
           05  WAN0-LL                           PIC S9(004) COMP.
           05  WAN0-ZZ                           PIC S9(004) COMP.
           05  WAN0-TRANSACAO                    PIC  X(009).
           05  WAN0-FUNCAO                       PIC  X(008).
           05  WAN0-DADOS-RST.
               07  WAN0-QTDE-OCOR                PIC  9(007).
               07  WAN0-QTDE-TOT-REG             PIC  9(009).
               07  WAN0-FUNC-BDSCO               PIC  X(007).
               07  WAN0-FILTRO.
                   09  WAN0-FLT-TIPO-HIST        PIC  X(001).
      *---             T = TODOS
      *---             P = PEDIDOS
      *---             D = DEFERIDOS NAO VIGENTES
                   09  WAN0-FLT-CAGPTO-CTA       PIC  9(003).
               07  WAN0-PONTEIRO.
                   09  WAN0-PONT-DESPR           PIC  9(005).
           05  WAN0-FIM                          PIC  X(001).
           05  WAN0-ERRO                         PIC  9(001).
           05  WAN0-COD-MSG-ERRO                 PIC  9(007).
           05  WAN0-COD-SQL-ERRO                 PIC  9(003).
           05  WAN0-DESC-MSG-ERRO                PIC  X(079).
           05  FILLER                            PIC  X(130).
           05  WAN0-DADOS.
               07  FILLER                        OCCURS 16.
                   09  WAN0-GRUPO-CTBIL          PIC  9(003).
                   09  WAN0-SUB-GRUPO-CTBIL      PIC  9(003).
                   09  WAN0-DESC-CTBIL           PIC  X(040).
