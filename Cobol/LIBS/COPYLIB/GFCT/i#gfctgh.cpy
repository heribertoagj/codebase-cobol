      *****************************************************************
      * NOME DA INC - I#GFCTGH                                        *
      * DESCRICAO   - DEFERIMENTO ESTORNO AGRUPAMENTO                 *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 0200                                            *
      * GFCTGH-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTGH-ENTRADA.
           03 GFCTGH-LL                            PIC S9(004) COMP.
           03 GFCTGH-ZZ                            PIC S9(004) COMP.
           03 GFCTGH-TRANSACAO                     PIC  X(009).
           03 GFCTGH-FUNCAO                        PIC  X(008).
           03 GFCTGH-DADOS-RST.
              05 GFCTGH-QTDE-OCOR                  PIC  9(003).
              05 GFCTGH-QTDE-TOT-REG               PIC  9(006).
              05 GFCTGH-FUNC-BDSCO                 PIC  X(007).
              05 GFCTGH-FILTRO.
                 07 GFCTGH-FILTRO-AGPTO            PIC  9(003).
                 07 GFCTGH-FILTRO-TARIFA           PIC  9(005).
              05 GFCTGH-PONTEIRO.
                 07 GFCTGH-PONT-AGPTO              PIC  9(003).
                 07 GFCTGH-PONT-TARIFA             PIC  9(005).
                 07 GFCTGH-PONT-DATA               PIC  X(010).
                 07 GFCTGH-PONT-TIMESTAMP          PIC  X(026).
              05 GFCTGH-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(110).
