      *****************************************************************
      * NOME DA INC - I#GFCT4S                                        *
      * DESCRICAO   - SEGURANCA - CONSULTA SE MASTER                  *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 0150                                            *
      * GFCT1L-LL   - 1010                                            *
      * DATA        - 19.01.2005                                      *
      * RESPONSAVEL - MARCELO  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCT4S-SAIDA.
           03  GFCT4S-LL                            PIC S9(004) COMP.
           03  GFCT4S-ZZ                            PIC S9(004) COMP.
           03  GFCT4S-TRANSACAO                     PIC  X(009).
           03  GFCT4S-FUNCAO                        PIC  X(008).
           03  GFCT4S-FUNC-BDSCO                    PIC  X(007).
           03  GFCT4S-CJUNC-DEPDC                   PIC  9(005).
           03  GFCT4S-TERMINAL                      PIC  X(008).
           03  GFCT4S-IDTFD-MASTER                  PIC  X(001).
      *        'S' - SIM   'N' - NAO
           03  GFCT4S-FIM                           PIC  X(001).
           03  GFCT4S-ERRO                          PIC  9(001).
           03  GFCT4S-COD-MSG-ERRO                  PIC  9(007).
           03  GFCT4S-COD-SQL-ERRO                  PIC  9(003).
           03  GFCT4S-DESC-MSG-ERRO                 PIC  X(079).
           03  FILLER                               PIC  X(016).
