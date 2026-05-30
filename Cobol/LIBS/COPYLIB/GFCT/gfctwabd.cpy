      ******************************************************************
      * NOME DA INC - GFCTWABD                                         *
      * DESCRICAO   - TEXT CORRESPONDENTE BANCARIO /                   *
      *               BANCO POSTAL OU BRADESCO EXPRESSO                *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 25/08/2008                                       *
      * RESPONSAVEL - MARCUS VINICIUS - PROCWORK - TEL 4506-2297       *
      ******************************************************************
      *
       01  WABD-SAIDA.
           03  WABD-LL                            PIC S9(004) COMP.
           03  WABD-ZZ                            PIC S9(004) COMP.
           03  WABD-TRANSACAO                     PIC  X(009).
           03  WABD-FUNCAO                        PIC  X(008).
           03  WABD-QTDE-OCOR                     PIC  9(003).
           03  WABD-QTDE-TOT-REG                  PIC  9(006).
           03  WABD-FUNC-BDSCO                    PIC  X(007).
           03  WABD-TERMINAL                      PIC  X(008).
           03  WABD-ERRO                          PIC  9(001).
           03  WABD-COD-MSG-ERRO                  PIC  9(007).
           03  WABD-COD-SQL-ERRO                  PIC  9(003).
           03  WABD-DESC-MSG-ERRO                 PIC  X(079).
           03  WABD-FIM                           PIC  X(001).
           03  FILLER                             PIC  X(069).
           03  WABD-DADOS.
               05  FILLER                           OCCURS 7.
                   07  WABD-COD-POSTO             PIC  9(005).
                   07  WABD-ERRO-POSTO            PIC  9(001).
                   07  WABD-DESC-POSTO            PIC  X(020).
                   07  WABD-COD-MSG-ERRO-GEN      PIC  9(007).
                   07  WABD-COD-SQL-ERRO-GEN      PIC  9(003).
                   07  WABD-DESC-MSG-ERRO-GEN     PIC  X(079).
