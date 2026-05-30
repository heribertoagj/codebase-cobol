      *****************************************************************
      * NOME DA INC - I#GFCT4R                                        *
      * DESCRICAO   - SEGURANCA - VERIFICAÇ O DE MASTER               *
      *               CONSULTAS                                       *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * GFCT4R-LL   - 1010                                            *
      * DATA        - 19.01.2005                                      *
      * RESPONSAVEL - MARCELO  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCT4R-ENTRADA.
           03  GFCT4R-LL                            PIC S9(004) COMP.
           03  GFCT4R-ZZ                            PIC S9(004) COMP.
           03  GFCT4R-TRANSACAO                     PIC  X(009).
           03  GFCT4R-FUNCAO                        PIC  X(008).
           03  GFCT4R-QTDE-OCOR                     PIC  9(003).
           03  GFCT4R-QTDE-TOT-REG                  PIC  9(006).
           03  GFCT4R-FUNC-BDSCO                    PIC  X(007).
           03  GFCT4R-CJUNC-DEPDC                   PIC  9(005).
           03  GFCT4R-TERMINAL                      PIC  X(008).
           03  GFCT4R-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(049).
