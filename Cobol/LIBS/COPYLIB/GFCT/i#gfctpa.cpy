      *****************************************************************
      * NOME DA INC - I#GFCTPA                                        *
      * DESCRICAO   - SUSPENSAO DE TARIFA - CONSULTA                  *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTPA-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTPA-ENTRADA.
           03 GFCTPA-LL                            PIC S9(004) COMP.
           03 GFCTPA-ZZ                            PIC S9(004) COMP.
           03 GFCTPA-TRANSACAO                     PIC  X(009).
           03 GFCTPA-FUNCAO                        PIC  X(008).
           03 GFCTPA-DADOS-RST.
              05 GFCTPA-QTDE-OCOR                  PIC  9(003).
              05 GFCTPA-QTDE-TOT-REG               PIC  9(006).
              05 GFCTPA-FUNC-BDSCO                 PIC  X(007).
              05 GFCTPA-PONTEIRO.
                 07 GFCTPA-PONT-AGPTO              PIC  9(003).
                 07 GFCTPA-PONT-TARIFA             PIC  9(005).
                 07 GFCTPA-PONT-CH-NUM5            PIC  9(005).
                 07 GFCTPA-PONT-CH-NUM13           PIC  9(013).
                 07 GFCTPA-PONT-CH-ALP             PIC  X(002).
                 07 GFCTPA-PONT-CH-DATA            PIC  X(010).
              05 GFCTPA-FIM                        PIC  X(001).
              05 GFCTPA-FILTRO-TARIFA              PIC  9(005).
              05 FILLER                            PIC  X(929).
