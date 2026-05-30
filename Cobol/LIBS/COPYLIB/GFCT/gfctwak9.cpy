      *****************************************************************
      * NOME DA INC - GFCTWAK9                                        *
      * DESCRICAO   - FLEXZ PARA AG/CONTA - LISTA CANCEL PEDIDOS P/LOTE
      *               COMUNICACAO WEB X IMS  - ENTRADA - GFCT5275     *
      * TAMANHO     - 200                                             *
      * WAK9-LL     - 200                                             *
      * DATA        - 17.02.2010                                      *
      * RESPONSAVEL - VANI     - PROCWORK                             *
      *****************************************************************
      *
       01  WAK9-ENTRADA.
           03  WAK9-LL                            PIC S9(004) COMP.
           03  WAK9-ZZ                            PIC S9(004) COMP.
           03  WAK9-TRANSACAO                     PIC  X(009).
           03  WAK9-FUNCAO                        PIC  X(008).
           03  WAK9-DADOS-RST.
               05  WAK9-QTDE-OCOR                 PIC  9(003).
               05  WAK9-QTDE-TOT-REG              PIC  9(006).
               05  WAK9-FUNC-BDSCO                PIC  X(007).
               05  WAK9-FILTRO.
                   07  WAK9-AGENCIA-FIL           PIC  9(005).
                   07  WAK9-CONTA-FIL             PIC  9(013).
                   07  WAK9-TARIFA-FIL            PIC  9(005).
                   07  WAK9-CONJ-SERV-FIL         PIC  9(003).
                   07  WAK9-COD-LOTE-FIL          PIC  9(011).
                   07  WAK9-CANC-TODAS-FIL        PIC  X(001).
               05  WAK9-PONTEIRO.
                   07  WAK9-AGENCIA-PONT          PIC  9(005).
                   07  WAK9-CONTA-PONT            PIC  9(013).
                   07  WAK9-TARIFA-PONT           PIC  9(005).
                   07  WAK9-CONJ-SERV-PONT        PIC  9(003).
                   07  WAK9-COD-LOTE-PONT         PIC  9(011).
                   07  WAK9-TIMESTAMP-PONT        PIC  X(026).
               05  WAK9-FIM                       PIC  X(001).
           03  FILLER                             PIC  X(061).
