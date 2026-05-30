      *****************************************************************
      * NOME DA INC - GFCTWANS                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - DEFER DETALHE    *
      *               COMUNICACAO WEB X IMS        - ENTRADA (0877)   *
      * TAMANHO     - 200                                             *
      * GFCTNS-LL   - 200                                             *
      * DATA        - 15.06.2010                                      *
      * RESPONSAVEL - CIBELE   - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTNS-ENTRADA.
           03  GFCTNS-LL                            PIC S9(004) COMP.
           03  GFCTNS-ZZ                            PIC S9(004) COMP.
           03  GFCTNS-TRANSACAO                     PIC  X(009).
           03  GFCTNS-FUNCAO                        PIC  X(008).
           03  GFCTNS-DADOS-RST.
               05  GFCTNS-QTDE-OCOR                 PIC  9(003).
               05  GFCTNS-QTDE-TOT-REG              PIC  9(006).
               05  GFCTNS-FUNC-BDSCO                PIC  X(007).
               05  GFCTNS-FILTRO.
                   07  GFCTNS-AGENCIA-FIL           PIC  9(005).
                   07  GFCTNS-CONTA-FIL             PIC  9(013).
                   07  GFCTNS-DATA-FIL              PIC  X(010).
                   07  GFCTNS-LOTE-FIL              PIC  9(011).
                   07  GFCTNS-TIMESTAMP-FIL         PIC  X(026).
                   07  GFCTNS-CONJ-SERV-FIL         PIC  9(003).
               05  GFCTNS-PONTEIRO.
                   07  GFCTNS-TARIFA-PONT           PIC  9(005).
               05  GFCTNS-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(089).
