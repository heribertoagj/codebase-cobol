      *****************************************************************
      * NOME DA INC - I#GFCTND                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO - LISTA CANCEL PEDIDOS   *
      *               COMUNICACAO WEB X IMS  - ENTRADA - GFCT0420     *
      * TAMANHO     - 200                                             *
      * GFCTND-LL   - 200                                             *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTND-ENTRADA.
           03  GFCTND-LL                            PIC S9(004) COMP.
           03  GFCTND-ZZ                            PIC S9(004) COMP.
           03  GFCTND-TRANSACAO                     PIC  X(009).
           03  GFCTND-FUNCAO                        PIC  X(008).
           03  GFCTND-DADOS-RST.
               05  GFCTND-QTDE-OCOR                 PIC  9(007).
               05  GFCTND-QTDE-TOT-REG              PIC  9(009).
               05  GFCTND-FUNC-BDSCO                PIC  X(007).
               05  GFCTND-FILTRO.
                   07  GFCTND-AGRUPMTO-FIL          PIC  9(003).
                   07  GFCTND-TARIFA-FIL            PIC  9(005).
               05  GFCTND-PONTEIRO.
                   07  GFCTND-AGRUPMTO-PONT         PIC  9(003).
                   07  GFCTND-TARIFA-PONT           PIC  9(005).
                   07  GFCTND-TIMESTAMP-PONT        PIC  X(026).
                   07  GFCTND-SEQUENCIA-PONT        PIC  9(002).
                   07  GFCTND-DATA-PONT             PIC  X(010).
               05  GFCTND-FIM                       PIC  X(001).
           03  FILLER                               PIC  X(101).
