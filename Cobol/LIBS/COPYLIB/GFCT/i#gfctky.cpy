      *****************************************************************
      * NOME DA INC - I#GFCTKY                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - DEFER DETALHE    *
      *               COMUNICACAO WEB X IMS        - ENTRADA (0875)   *
      * TAMANHO     - 200                                             *
      * GFCTKY-LL   - 200                                             *
      * DATA        - 15.02.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTKY-ENTRADA.
           03  GFCTKY-LL                            PIC S9(004) COMP.
           03  GFCTKY-ZZ                            PIC S9(004) COMP.
           03  GFCTKY-TRANSACAO                     PIC  X(009).
           03  GFCTKY-FUNCAO                        PIC  X(008).
           03  GFCTKY-DADOS-RST.
               05  GFCTKY-QTDE-OCOR                 PIC  9(003).
               05  GFCTKY-QTDE-TOT-REG              PIC  9(006).
               05  GFCTKY-FUNC-BDSCO                PIC  X(007).
               05  GFCTKY-CONS-DET.
                   07  GFCTKY-AGENCIA-CONS          PIC  9(005).
                   07  GFCTKY-CONTA-CONS            PIC  9(013).
                   07  GFCTKY-TARIFA-CONS           PIC  9(005).
                   07  GFCTKY-TIMESTAMP-CONS        PIC  X(026).
                   07  GFCTKY-DATA-CONS             PIC  X(010).
                   07  GFCTKY-CONJ-SERV             PIC  9(003).
                   07  GFCTKY-LOTE                  PIC  9(011).
               05  GFCTKY-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(089).
