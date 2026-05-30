      *****************************************************************
      * NOME DA INC - I#GFCTN8                                        *
      * DESCRICAO   - ADESAO COMPULSORIA - COMBO PACOTE               *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTN8-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTN8-SAIDA.
           03 GFCTN8-LL                            PIC S9(004) COMP.
           03 GFCTN8-ZZ                            PIC S9(004) COMP.
           03 GFCTN8-TRANSACAO                     PIC  X(009).
           03 GFCTN8-FUNCAO                        PIC  X(008).
           03 GFCTN8-DADOS-RST.
              05 GFCTN8-QTDE-OCOR                  PIC  9(003).
              05 GFCTN8-QTDE-TOT-REG               PIC  9(006).
              05 GFCTN8-FUNC-BDSCO                 PIC  X(007).
              05 GFCTN8-PONTEIRO.
                 07 GFCTN8-PONT-AGPTO              PIC  9(003).
                 07 GFCTN8-PONT-PACOTE             PIC  9(005).
                 07 GFCTN8-PONT-CH-NUM5            PIC  9(005).
                 07 GFCTN8-PONT-CH-NUM13           PIC  9(013).
                 07 GFCTN8-PONT-CH-ALP             PIC  X(002).
              05 GFCTN8-IND-PESSOA                 PIC  X(001).
      *       F - FISICO  J - JURIDICO A - AMBAS
              05 GFCTN8-ERRO                       PIC  9(001).
              05 GFCTN8-COD-MSG-ERRO               PIC  9(007).
              05 GFCTN8-COD-SQL-ERRO               PIC  9(003).
              05 GFCTN8-DESC-MSG-ERRO              PIC  X(079).
              05 GFCTN8-FIM                        PIC  X(001).
BI0810        05 FILLER                            PIC  X(193).
           03 GFCTN8-OCORR                         OCCURS 10 TIMES.
              05 GFCTN8-PACOTE                     PIC  9(005).
              05 GFCTN8-DESC-PACOTE                PIC  X(040).
              05 GFCTN8-DATA-INICIO                PIC  X(010).
              05 GFCTN8-DATA-FIM                   PIC  X(010).
BI0810        05 GFCTN8-CPSSOA-TARIFA              PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
