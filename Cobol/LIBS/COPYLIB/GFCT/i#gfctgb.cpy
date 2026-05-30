      ******************************************************************
      * NOME DA INC - I#GFCTGB                                         *
      * DESCRICAO   - TEXT DE CORRESPONDENTE BANCARIO / PAB            *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 14/09/2005                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTGB-ENTRADA.
           03  GFCTGB-LL                            PIC S9(004) COMP.
           03  GFCTGB-ZZ                            PIC S9(004) COMP.
           03  GFCTGB-TRANSACAO                     PIC  X(009).
           03  GFCTGB-FUNCAO                        PIC  X(008).
           03  GFCTGB-DADOS-RST.
               05  GFCTGB-QTDE-OCOR                     PIC  9(003).
               05  GFCTGB-QTDE-TOT-REG                  PIC  9(006).
               05  GFCTGB-FUNC-BDSCO                    PIC  X(007).
               05  GFCTGB-TERMINAL                      PIC  X(008).
               05  GFCTGB-FIM                           PIC  X(001).
               05  GFCTGB-COD-DEPDC                     PIC  9(005).
               05  GFCTGB-COD-POSTO                     PIC  9(005).
               05  FILLER                               PIC  X(044).
