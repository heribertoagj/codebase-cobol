      *****************************************************************
      * NOME DA INC - GFCTWANU                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG./CONTA - CONSUL DETALHE  *
      *               COMUNICACAO WEB X IMS         - SAIDA  (0876)   *
      * TAMANHO     - 1010                                            *
      * GFCTNU-LL   - 1010                                            *
      * DATA        - 21.06.2010                                      *
      * RESPONSAVEL - CIBELE  - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNU-SAIDA.
           03  GFCTNU-LL                            PIC S9(004) COMP.
           03  GFCTNU-ZZ                            PIC S9(004) COMP.
           03  GFCTNU-TRANSACAO                     PIC  X(009).
           03  GFCTNU-FUNCAO                        PIC  X(008).
           03  GFCTNU-QTDE-OCOR                     PIC  9(003).
           03  GFCTNU-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTNU-FUNC-BDSCO                    PIC  X(007).
           03  GFCTNU-ERRO                          PIC  9(001).
           03  GFCTNU-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNU-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNU-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTNU-FIM                           PIC  X(001).
JUN10      03  FILLER                               PIC  X(646).
           03  GFCTNU-DADOS-HIST.
               05  GFCTNU-AGENCIA                   PIC  9(005).
               05  GFCTNU-CONTA                     PIC  9(013).
               05  GFCTNU-COD-LOTE                  PIC  9(011).
JUN10          05  GFCTNU-TARIFAS    OCCURS 20.
                   07  GFCTNU-COD-TARIFA            PIC  9(005).
JUN10              07  GFCTNU-COD-MSG-ERRO-TAR      PIC  9(004).
      * IGUAL A 0 = SEM ERRO
      * DIFERENTE DE 0 = COM-ERRO
               05  GFCTNU-TIMESTAMP                 PIC  X(026).
               05  GFCTNU-OPERACAO                  PIC  X(001).
      * D = USUARIO PODE DEFERIR
      * I = USUARIO PODE INDEFERIR OU TRANSFERIR
