      *****************************************************************
      * NOME DA INC - GFCTWAK2                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - FLEXIBILIZACAO               *
      *               COMBO DE LOTE- DEFERIMENTO FLEXIBILIZACAO       *
      *               POR AG/CONTA                                    *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                 GFCT5213   *
      * DATA        - 25.01.2010                                      *
      * RESPONSAVEL - CIBELE          - PROCWORK                      *
      *****************************************************************
      *
       01  WAK2-SAIDA.
           03  WAK2-LL                           PIC S9(004) COMP.
           03  WAK2-ZZ                           PIC S9(004) COMP.
           03  WAK2-TRANSACAO                    PIC  X(009).
           03  WAK2-FUNCAO                       PIC  X(008).
           03  WAK2-FILTRO.
               05  WAK2-FLT-CJUNC-DEPDC          PIC  9(005).
               05  WAK2-FLT-CCTA-CLI             PIC  9(013).
               05  WAK2-FLT-CONJ-SERV            PIC  9(003).
           03  WAK2-QTDE-OCOR                    PIC  9(003).
           03  WAK2-QTDE-TOT-REG                 PIC  9(006).
           03  WAK2-FUNC-BDSCO                   PIC  X(007).
           03  WAK2-PONTEIRO.
               05  WAK2-PONT-DESPREZ             PIC  9(005).
           03  WAK2-FIM                          PIC  X(001).
           03  WAK2-ERRO                         PIC  9(001).
           03  WAK2-COD-MSG-ERRO                 PIC  9(007).
           03  WAK2-COD-SQL-ERRO                 PIC  9(003).
           03  WAK2-DESC-MSG-ERRO                PIC  X(079).
           03  FILLER                            PIC  X(119).
           03  WAK2-DADOS.
               05  FILLER                      OCCURS 67.
                   07  WAK2-COD-LOTE             PIC  9(011).
