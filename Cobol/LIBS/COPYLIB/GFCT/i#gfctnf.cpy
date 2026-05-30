      *****************************************************************
      * NOME DA INC - I#GFCTNF                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO - LISTA ENCER  PEDIDOS   *
      *               COMUNICACAO WEB X IMS  - ENTRADA - GFCT0882     *
      * TAMANHO     - 200                                             *
      * GFCTNF-LL   - 200                                             *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTNF-ENTRADA.
           03  GFCTNF-LL                            PIC S9(004) COMP.
           03  GFCTNF-ZZ                            PIC S9(004) COMP.
           03  GFCTNF-TRANSACAO                     PIC  X(009).
           03  GFCTNF-FUNCAO                        PIC  X(008).
           03  GFCTNF-DADOS-RST.
               05  GFCTNF-QTDE-OCOR                 PIC  9(007).
               05  GFCTNF-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNF-FUNC-BDSCO                PIC  X(007).
               05  GFCTNF-FILTRO.
                   07  GFCTNF-AGRUPMTO-FIL          PIC  9(003).
                   07  GFCTNF-TARIFA-FIL            PIC  9(005).
               05  GFCTNF-PONTEIRO.
                   07  GFCTNF-DATA-PONT             PIC  X(010).
                   07  GFCTNF-TARIFA-PONT           PIC  9(005).
                   07  GFCTNF-AGRUPMTO-PONT         PIC  9(003).
                   07  GFCTNF-TIMESTAMP-PONT        PIC  X(026).
               05  GFCTNF-FIM                       PIC  X(001).
           03  FILLER                               PIC  X(103).
