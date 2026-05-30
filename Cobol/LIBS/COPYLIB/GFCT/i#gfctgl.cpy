      *****************************************************************
      * NOME DA INC - I#GFCTGL                                        *
      * DESCRICAO   - CONSULTA ESTORNO AGRUPAMENTO                    *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 0200                                            *
      * GFCTGL-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTGL-ENTRADA.
           03 GFCTGL-LL                            PIC S9(004) COMP.
           03 GFCTGL-ZZ                            PIC S9(004) COMP.
           03 GFCTGL-TRANSACAO                     PIC  X(009).
           03 GFCTGL-FUNCAO                        PIC  X(008).
           03 GFCTGL-DADOS-RST.
              05 GFCTGL-QTDE-OCOR                  PIC  9(003).
              05 GFCTGL-QTDE-TOT-REG               PIC  9(006).
              05 GFCTGL-FUNC-BDSCO                 PIC  X(007).
              05 GFCTGL-FILTRO.
                 07 GFCTGL-FILTRO-AGPTO            PIC  9(003).
                 07 GFCTGL-FILTRO-TARIFA           PIC  9(005).
              05 GFCTGL-PONTEIRO.
                 07 GFCTGL-PONT-AGPTO              PIC  9(003).
                 07 GFCTGL-PONT-TARIFA             PIC  9(005).
                 07 GFCTGL-PONT-DATA               PIC  X(010).
                 07 GFCTGL-PONT-TIMESTAMP          PIC  X(026).
              05 GFCTGL-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(110).
