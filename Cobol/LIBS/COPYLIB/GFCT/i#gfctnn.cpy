      *****************************************************************
      * NOME DA INC - I#GFCTNN                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AGRUPAMENTO - LISTA DEFERIM *
      *               COMUNICACAO WEB X IMS           - ENTRADA (0425)*
      * TAMANHO     - 200                                             *
      * GFCTNN-LL   - 200                                             *
      * DATA        - 25.03.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTNN-ENTRADA.
           03  GFCTNN-LL                            PIC S9(004) COMP.
           03  GFCTNN-ZZ                            PIC S9(004) COMP.
           03  GFCTNN-TRANSACAO                     PIC  X(009).
           03  GFCTNN-FUNCAO                        PIC  X(008).
           03  GFCTNN-DADOS-RST.
               05  GFCTNN-QTDE-OCOR                 PIC  9(003).
               05  GFCTNN-QTDE-TOT-REG              PIC  9(006).
               05  GFCTNN-FUNC-BDSCO                PIC  X(007).
               05  GFCTNN-FILTRO.
                   07  GFCTNN-AGRUPAMENTO-FIL       PIC  9(003).
                   07  GFCTNN-DATA-FIL              PIC  X(010).
               05  GFCTNN-PONTEIRO.
                   07  GFCTNN-AGRUPAMENTO-PONT      PIC  9(003).
                   07  GFCTNN-TARIFA-PONT           PIC  9(005).
                   07  GFCTNN-TIMESTAMP-PONT        PIC  X(026).
                   07  GFCTNN-SEQ-PONT              PIC  9(002).
                   07  GFCTNN-DATA-PONT             PIC  X(010).
               05  GFCTNN-FIM                       PIC  X(001).
           03  FILLER                               PIC  X(103).
