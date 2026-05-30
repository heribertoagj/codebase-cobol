      *****************************************************************
      * NOME DA INC - I#GFCTKV                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - DEFERIMENTO      *
      *               COMUNICACAO WEB X IMS        - ENTRADA (0424)   *
      * TAMANHO     - 200                                             *
      * GFCTKV-LL   - 200                                             *
      * DATA        - 15.02.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTKV-ENTRADA.
           03  GFCTKV-LL                            PIC S9(004) COMP.
           03  GFCTKV-ZZ                            PIC S9(004) COMP.
           03  GFCTKV-TRANSACAO                     PIC  X(009).
           03  GFCTKV-FUNCAO                        PIC  X(008).
           03  GFCTKV-DADOS-RST.
               05  GFCTKV-QTDE-OCOR                 PIC  9(003).
               05  GFCTKV-QTDE-TOT-REG              PIC  9(006).
               05  GFCTKV-FUNC-BDSCO                PIC  X(007).
               05  GFCTKV-FILTRO.
                   07  GFCTKV-AGENCIA-FIL           PIC  9(005).
                   07  GFCTKV-CONTA-FIL             PIC  9(013).
                   07  GFCTKV-DATA-FIL              PIC  X(010).
                   07  GFCTKV-LOTE-FIL              PIC  9(011).
                   07  GFCTKV-CONJ-SERV-FIL         PIC  9(003).
               05  GFCTKV-PONTEIRO.
                   07  GFCTKV-AGENCIA-PONT          PIC  9(005).
                   07  GFCTKV-CONTA-PONT            PIC  9(013).
                   07  GFCTKV-TARIFA-PONT           PIC  9(005).
                   07  GFCTKV-TIMESTAMP-PONT        PIC  X(026).
               05  GFCTKV-FIM                       PIC  X(001).
           03  FILLER                               PIC  X(071).
