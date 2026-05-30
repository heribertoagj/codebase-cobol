      *****************************************************************
      * NOME DA INC - I#GFCTN7                                        *
      * DESCRICAO   - ADESAO COMPULSORIA - COMBO PACOTE.              *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTN7-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTN7-ENTRADA.
           03 GFCTN7-LL                            PIC S9(004) COMP.
           03 GFCTN7-ZZ                            PIC S9(004) COMP.
           03 GFCTN7-TRANSACAO                     PIC  X(009).
           03 GFCTN7-FUNCAO                        PIC  X(008).
           03 GFCTN7-DADOS-RST.
              05 GFCTN7-QTDE-OCOR                  PIC  9(003).
              05 GFCTN7-QTDE-TOT-REG               PIC  9(006).
              05 GFCTN7-FUNC-BDSCO                 PIC  X(007).
              05 GFCTN7-PONTEIRO.
                 07 GFCTN7-PONT-AGPTO              PIC  9(003).
                 07 GFCTN7-PONT-PACOTE             PIC  9(005).
                 07 GFCTN7-PONT-CH-NUM5            PIC  9(005).
                 07 GFCTN7-PONT-CH-NUM13           PIC  9(013).
                 07 GFCTN7-PONT-CH-ALP             PIC  X(002).
              05 GFCTN7-IND-PESSOA                 PIC  X(001).
      *       F - FISICO  J - JURIDICO A - AMBAS
              05 GFCTN7-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(943).
