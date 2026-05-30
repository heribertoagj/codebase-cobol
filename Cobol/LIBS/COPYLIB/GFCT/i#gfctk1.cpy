      *****************************************************************
      * NOME DA INC - I#GFCTK1                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - LUPA - GFCT5116  *
      *               COMUNICACAO WEB X IMS        - ENTRADA          *
      * TAMANHO     - 200                                             *
      * GFCTK1-LL   - 200                                             *
      * DATA        - 02.02.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTK1-ENTRADA.
           03  GFCTK1-LL                            PIC S9(004) COMP.
           03  GFCTK1-ZZ                            PIC S9(004) COMP.
           03  GFCTK1-TRANSACAO                     PIC  X(009).
           03  GFCTK1-FUNCAO                        PIC  X(008).
           03  GFCTK1-DADOS-RST.
               05  GFCTK1-QTDE-OCOR                 PIC  9(003).
               05  GFCTK1-QTDE-TOT-REG              PIC  9(006).
               05  GFCTK1-FUNC-BDSCO                PIC  X(007).
               05  GFCTK1-TERMINAL                  PIC  X(008).
               05  GFCTK1-DADOS1.
                   07  GFCTK1-AGENCIA               PIC  9(005).
                   07  GFCTK1-CONTA                 PIC  9(007).
               05  GFCTK1-FIM                       PIC  X(001).
               05 FILLER                            PIC  X(142).
