      ******************************************************************
      * NOME DA INC - I#GFCTGC                                         *
      * DESCRICAO   - TEXT CORRESPONDENTE BANCARIO / PAB               *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 14/09/2005                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTGC-SAIDA.
           03  GFCTGC-LL                            PIC S9(004) COMP.
           03  GFCTGC-ZZ                            PIC S9(004) COMP.
           03  GFCTGC-TRANSACAO                     PIC  X(009).
           03  GFCTGC-FUNCAO                        PIC  X(008).
           03  GFCTGC-QTDE-OCOR                     PIC  9(003).
           03  GFCTGC-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTGC-FUNC-BDSCO                    PIC  X(007).
           03  GFCTGC-TERMINAL                      PIC  X(008).
           03  GFCTGC-ERRO                          PIC  9(001).
           03  GFCTGC-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTGC-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTGC-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTGC-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(069).
           03  GFCTGC-DADOS.
               05  FILLER                           OCCURS 7.
                   07  GFCTGC-COD-POSTO             PIC  9(005).
                   07  GFCTGC-ERRO-POSTO            PIC  9(001).
                   07  GFCTGC-DESC-POSTO            PIC  X(020).
                   07  GFCTGC-COD-MSG-ERRO-GEN      PIC  9(007).
                   07  GFCTGC-COD-SQL-ERRO-GEN      PIC  9(003).
                   07  GFCTGC-DESC-MSG-ERRO-GEN     PIC  X(079).
