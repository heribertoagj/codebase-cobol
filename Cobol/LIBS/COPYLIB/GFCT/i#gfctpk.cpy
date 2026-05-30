      *****************************************************************
      * NOME DA INC - I#GFCTPK                                        *
      * DESCRICAO   - SUSPENSAO DE TARIFA -COMBO ARGUMENTOS           *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTPK-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTPK-ENTRADA.
           03 GFCTPK-LL                            PIC S9(004) COMP.
           03 GFCTPK-ZZ                            PIC S9(004) COMP.
           03 GFCTPK-TRANSACAO                     PIC  X(009).
           03 GFCTPK-FUNCAO                        PIC  X(008).
           03 GFCTPK-DADOS-RST.
              05 GFCTPK-QTDE-OCOR                  PIC  9(003).
              05 GFCTPK-QTDE-TOT-REG               PIC  9(006).
              05 GFCTPK-FUNC-BDSCO                 PIC  X(007).
              05 GFCTPK-FILTRO.
                 07 GFCTPK-FILTRO-AGPTO            PIC  9(003).
                 07 GFCTPK-FILTRO-TARIFA           PIC  9(005).
              05 GFCTPK-PONTEIRO.
                 07 GFCTPK-PONT-AGPTO              PIC  9(003).
                 07 GFCTPK-PONT-TARIFA             PIC  9(005).
                 07 GFCTPK-PONT-SEQ                PIC  9(009).
              05 GFCTPK-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(947).
