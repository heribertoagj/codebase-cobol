      *****************************************************************
      * NOME DA INC - I#GFCTJB                                        *
      * DESCRICAO   - FLEXZ PARA AG/CONTA - LISTA CANCEL PEDIDOS      *
      *               COMUNICACAO WEB X IMS  - ENTRADA - GFCT0405     *
      * TAMANHO     - 200                                             *
      * GFCTJB-LL   - 200                                             *
      * DATA        - 28.12.2005                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTJB-ENTRADA.
           03  GFCTJB-LL                            PIC S9(004) COMP.
           03  GFCTJB-ZZ                            PIC S9(004) COMP.
           03  GFCTJB-TRANSACAO                     PIC  X(009).
           03  GFCTJB-FUNCAO                        PIC  X(008).
           03  GFCTJB-DADOS-RST.
               05  GFCTJB-QTDE-OCOR                 PIC  9(003).
               05  GFCTJB-QTDE-TOT-REG              PIC  9(006).
               05  GFCTJB-FUNC-BDSCO                PIC  X(007).
               05  GFCTJB-FILTRO.
                   07  GFCTJB-AGENCIA-FIL           PIC  9(005).
                   07  GFCTJB-CONTA-FIL             PIC  9(013).
                   07  GFCTJB-TARIFA-FIL            PIC  9(005).
               05  GFCTJB-PONTEIRO.
                   07  GFCTJB-AGENCIA-PONT          PIC  9(005).
                   07  GFCTJB-CONTA-PONT            PIC  9(013).
                   07  GFCTJB-TARIFA-PONT           PIC  9(005).
                   07  GFCTJB-TIMESTAMP-PONT        PIC  X(026).
               05  GFCTJB-FIM                       PIC  X(001).
           03  FILLER                               PIC  X(090).
