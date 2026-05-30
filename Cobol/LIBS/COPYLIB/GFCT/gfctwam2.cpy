      *****************************************************************
      * NOME DA INC - GFCTWAM2                                        *
      * DESCRICAO   - CONSISTENCIA BCO POSTAL/BCO EXPRESSO            *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * WAM2-LL     - 100                                GFCT5296     *
      * DATA        - MARCO/2010                                      *
      * RESPONSAVEL - WELLEN FRANCA   - PROCWORK                      *
      *****************************************************************
       01  WAM2-ENTRADA.
           03 WAM2-LL                            PIC S9(004) COMP.
           03 WAM2-ZZ                            PIC S9(004) COMP.
           03 WAM2-TRANSACAO                     PIC  X(009).
           03 WAM2-FUNCAO                        PIC  X(008).
           03 WAM2-DADOS-RST.
              05 WAM2-FUNC-BDSCO                 PIC  X(007).
              05 WAM2-TERMINAL                   PIC  X(008).
              05 WAM2-FIM                        PIC  X(001).
              05 WAM2-FILLER                     PIC  X(050).
           03 WAM2-FIXO.
              05 WAM2-CAGPTO                     PIC  9(003).
              05 WAM2-CAGENCIA                   PIC  9(005).
              05 WAM2-CPAB                       PIC  9(005).

