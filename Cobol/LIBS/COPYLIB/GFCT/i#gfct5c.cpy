      *****************************************************************
      * NOME DA INC - I#GFCT5C                                        *
      * DESCRICAO   - SEGURANCA - LISTA ASSOSSIACAO DE GRUPO          *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 500                                             *
      * GFCT5C-LL   - 1010                                            *
      * DATA        - 03.02.2005                                      *
      * RESPONSAVEL - MARCELO  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCT5C-ENTRADA.
           03  GFCT5C-LL                            PIC S9(004) COMP.
           03  GFCT5C-ZZ                            PIC S9(004) COMP.
           03  GFCT5C-TRANSACAO                     PIC  X(009).
           03  GFCT5C-FUNCAO                        PIC  X(008).
           03  GFCT5C-DADOS-RST.
               05  GFCT5C-QTDE-OCOR                 PIC  9(003).
               05  GFCT5C-QTDE-TOT-REG              PIC  9(006).
               05  GFCT5C-FUNC-BDSCO                PIC  X(007).
               05  GFCT5C-TERMINAL                  PIC  X(008).
               05  GFCT5C-PONTEIRO                  PIC  9(005).
               05  GFCT5C-GRUPO-LANCTO              PIC  9(005).
               05  GFCT5C-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(029).
           03  GFCT5C-DADOS-LISTA                   OCCURS 5 TIMES.
               05  GFCT5C-COD-LANCTO                PIC  9(005).
               05  GFCT5C-DESC-LANCTO               PIC  X(024).
               05  GFCT5C-COD-PERIF                 PIC  9(003).
               05  GFCT5C-DESC-PERIF                PIC  X(030).
               05  GFCT5C-DEB-CRED-AMB              PIC  9(001).
    ********** 1 - CRED    2 - DEB                  0 - AMBOS
               05  GFCT5C-DINI-VIG                  PIC  X(010).
               05  GFCT5C-DFIM-VIG                   PIC  X(010).
