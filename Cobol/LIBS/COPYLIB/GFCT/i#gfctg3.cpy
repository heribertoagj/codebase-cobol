      *****************************************************************
      * NOME DA INC - I#GFCTG3                                        *
      * DESCRICAO   - MODULO - OBTER MENSAGEM                         *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 10.09.2005                                      *
      * RESPONSAVEL - MAURICIO - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTG3-SAIDA.
           03  GFCTG3-LL                            PIC S9(004) COMP.
           03  GFCTG3-ZZ                            PIC S9(004) COMP.
           03  GFCTG3-TRANSACAO                     PIC  X(009).
           03  GFCTG3-FUNCAO                        PIC  X(008).
           03  GFCTG3-FUNC-BDSCO                    PIC  X(007).
           03  GFCTG3-DESC-MSG                      PIC  X(070).
           03  GFCTG3-ERRO                          PIC  9(001).
           03  GFCTG3-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTG3-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTG3-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTG3-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(821).
