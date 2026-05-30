      *****************************************************************
      * NOME DA INC - I#GFCT5B                                        *
      * DESCRICAO   - DADOS BASICOS - GRUPO DE LANCAMENTOS            *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCT1L-LL   - 1010                                            *
      * DATA        - 12.02.2005                                      *
      * RESPONSAVEL - MARCELO  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCT5B-SAIDA.
           03  GFCT5B-LL                            PIC S9(004) COMP.
           03  GFCT5B-ZZ                            PIC S9(004) COMP.
           03  GFCT5B-TRANSACAO                     PIC  X(009).
           03  GFCT5B-FUNCAO                        PIC  X(008).
           03  GFCT5B-FILTRO                        PIC  9(005).
           03  GFCT5B-QTDE-OCOR                     PIC  9(003).
           03  GFCT5B-QTDE-TOT-REG                  PIC  9(006).
           03  GFCT5B-FUNC-BDSCO                    PIC  X(007).
           03  GFCT5B-TERMINAL                      PIC  X(008).
           03  GFCT5B-PONTEIRO                      PIC  9(005).
           03  GFCT5B-ERRO                          PIC  9(001).
           03  GFCT5B-COD-MSG-ERRO                  PIC  9(007).
           03  GFCT5B-COD-SQL-ERRO                  PIC  9(003).
           03  GFCT5B-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCT5B-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(059).
           03  GFCT5B-DADOS.
               05  FILLER                           OCCURS 7.
                   07  GFCT5B-COD-GRP               PIC  9(005).
                   07  GFCT5B-DESC-GRP              PIC  X(030).
                   07  GFCT5B-ERRO-OCOR             PIC  9(001).
    ********** ZERO=OK   1=ERRO
                   07  GFCT5B-DESC-MSG-ERRO-OCOR    PIC  X(079).

