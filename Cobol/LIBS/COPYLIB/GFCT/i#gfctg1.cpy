      *****************************************************************
      * NOME DA INC - I#GFCTG1                                        *
      * DESCRICAO   - MODULO - DISPONIBILIDADE DO SISTEMA             *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 10.09.2005                                      *
      * RESPONSAVEL - MAURICIO - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTG1-SAIDA.
           03  GFCTG1-LL                            PIC S9(004) COMP.
           03  GFCTG1-ZZ                            PIC S9(004) COMP.
           03  GFCTG1-TRANSACAO                     PIC  X(009).
           03  GFCTG1-FUNCAO                        PIC  X(008).
           03  GFCTG1-FUNC-BDSCO                    PIC  X(007).
           03  GFCTG1-CSIT-ONLINE                   PIC  9(001).
           03  GFCTG1-ERRO                          PIC  9(001).
           03  GFCTG1-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTG1-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTG1-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTG1-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(890).
