      *****************************************************************
      * NOME DA INC - GFCTWAM3                                        *
      * DESCRICAO   - CONSISTENCIA BCO POSTAL/BD EXPRESSO             *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * WAM3-LL     - 1010                                 GFCT5296   *
      * DATA        - MARCO/2010                                      *
      * RESPONSAVEL - WELLEN FRANCA   - PROCWORK                      *
      *****************************************************************
       01  WAM3-SAIDA.
           03 WAM3-LL                            PIC S9(004) COMP.
           03 WAM3-ZZ                            PIC S9(004) COMP.
           03 WAM3-TRANSACAO                     PIC  X(009).
           03 WAM3-FUNCAO                        PIC  X(008).
           03 WAM3-DADOS-RST.
              05 WAM3-FUNC-BDSCO                 PIC  X(007).
              05 WAM3-TERMINAL                   PIC  X(008).
              05 WAM3-FIM                        PIC  X(001).
           03 WAM3-ERRO                          PIC  9(001).
           03 WAM3-COD-MSG-ERRO                  PIC  9(007).
           03 WAM3-COD-SQL-ERRO                  PIC  9(003).
           03 WAM3-DESC-MSG-ERRO                 PIC  X(079).
           03 WAM3-FILLER                        PIC  X(850).
           03 WAM3-FIXO.
              05 WAM3-CAGPTO                     PIC  9(003).
              05 WAM3-CAGENCIA                   PIC  9(005).
              05 WAM3-CPAB                       PIC  9(005).
              05 WAM3-DESC-POSTO                 PIC  X(020).
