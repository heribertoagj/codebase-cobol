      *****************************************************************
      *
      *****************************************************************
      * NOME DA INC - I#GFCT5D                                        *
      * DESCRICAO   - SEGURANCA - LISTA ASSOSSIACAO DE GRUPO          *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                             *
      * GFCT5D-LL   - 1010                                            *
      * DATA        - 03.02.2005                                      *
      * RESPONSAVEL - MARCELO  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCT5D-SAIDA.
           03  GFCT5D-LL                        PIC S9(004) COMP.
           03  GFCT5D-ZZ                        PIC S9(004) COMP.
           03  GFCT5D-TRANSACAO                 PIC  X(009).
           03  GFCT5D-FUNCAO                    PIC  X(008).
           03  GFCT5D-QTDE-OCOR                 PIC  9(003).
           03  GFCT5D-QTDE-TOT-REG              PIC  9(006).
           03  GFCT5D-FUNC-BDSCO                PIC  X(007).
           03  GFCT5D-TERMINAL                  PIC  X(008).
           03  GFCT5D-PONTEIRO                  PIC  9(005).
           03  GFCT5D-GRUPO-LANCTO              PIC  9(005).
           03  GFCT5D-ERRO                      PIC  9(001).
           03  GFCT5D-COD-MSG-ERRO              PIC  9(007).
           03  GFCT5D-COD-SQL-ERRO              PIC  9(003).
           03  GFCT5D-DESC-MSG-ERRO             PIC  X(079).
           03  GFCT5D-FIM                       PIC  X(001).
           03  FILLER                           PIC  X(049).
           03  GFCT5D-DADOS-LISTA               OCCURS 05 TIMES.
               05  GFCT5D-COD-LANCTO            PIC  9(005).
               05  GFCT5D-DESC-LANCTO           PIC  X(024).
               05  GFCT5D-COD-PERIF             PIC  9(003).
               05  GFCT5D-DESC-PERIF            PIC  X(030).
               05  GFCT5D-DEB-CRED-AMB          PIC  9(001).
    ********** 1 - CRED       2 - DEB           0 - AMBOS
               05  GFCT5D-DINI-VIG              PIC  X(010).
               05  GFCT5D-DFIM-VIG              PIC  X(010).
               05  GFCT5D-ERRO-OCOR             PIC  9(001).
    ********** ZERO=OK   1=ERRO
               05  GFCT5D-DESC-MSG-ERRO-OCOR    PIC  X(079).
