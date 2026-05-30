      *****************************************************************
      * NOME DA INC - GFCTWAKX                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - DEFERIMENTO      *
      *               COMUNICACAO WEB X IMS        - ENTRADA          *
JUN10 * TAMANHO     - 300                                             *
      * WAKX-LL     - 300                                 GFCT0991    *
      * DATA        - 23.01.2010                                      *
      * RESPONSAVEL - CIBELE - SONDA PROCWORK                         *
      *****************************************************************
      *
       01  WAKX-ENTRADA.
           03 WAKX-LL                            PIC S9(004) COMP.
           03 WAKX-ZZ                            PIC S9(004) COMP.
           03 WAKX-TRANSACAO                     PIC  X(009).
           03 WAKX-FUNCAO                        PIC  X(008).
           03 WAKX-DADOS-RST.
               05 WAKX-QTDE-OCOR                  PIC  9(003).
               05 WAKX-QTDE-TOT-REG               PIC  9(006).
               05 WAKX-FUNC-BDSCO                 PIC  X(007).
               05 WAKX-FILTRO.
                   07 WAKX-AGENCIA-FIL            PIC  9(005).
                   07 WAKX-CONTA-FIL              PIC  9(013).
                   07 WAKX-TIMESTAMP-FIL          PIC  X(026).
                   07 WAKX-CFUNC-FIL              PIC  9(009).
                   07 WAKX-LOTE-FIL               PIC  9(011).
JUN10              07 WAKX-CTARIFAS OCCURS 20 TIMES.
                      10 WAKX-CTARIFA-LOTE        PIC  9(005).
           03  WAKX-ACESSO                        PIC  X(001).
      ***     'V' - VISUALIZAR IMPRESSAO
      ***     'I' - IMPRIME
           03  WAKX-TIMESTAMP                     PIC  X(026).
           03  WAKX-FIM                           PIC  X(001).
      ***     'S'
      ***     'N'
JUN10      03  FILLER                             PIC  X(071).
