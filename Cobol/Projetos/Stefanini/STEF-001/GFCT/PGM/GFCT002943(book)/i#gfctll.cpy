      *****************************************************************
      * NOME DA INC - I#GFCTLL                                        *
      * DESCRICAO   - ADESAO COMPULSORIA - CONSULTA                   *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTLL-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTLL-ENTRADA.
           03 GFCTLL-LL                            PIC S9(004) COMP.
           03 GFCTLL-ZZ                            PIC S9(004) COMP.
           03 GFCTLL-TRANSACAO                     PIC  X(009).
           03 GFCTLL-FUNCAO                        PIC  X(008).
           03 GFCTLL-DADOS-RST.
              05 GFCTLL-QTDE-OCOR                  PIC  9(003).
              05 GFCTLL-QTDE-TOT-REG               PIC  9(006).
              05 GFCTLL-FUNC-BDSCO                 PIC  X(007).
              05 GFCTLL-PONTEIRO.
                 07 GFCTLL-PONT-AGPTO              PIC  9(003).
                 07 GFCTLL-PONT-PACOTE             PIC  9(005).
                 07 GFCTLL-PONT-CH-NUM5            PIC  9(005).
                 07 GFCTLL-PONT-CH-NUM13           PIC  9(013).
ST2509           07 GFCTLL-PONT-CH-CCGC-CPF        PIC  X(009).
                 07 GFCTLL-PONT-CH-ALP             PIC  X(002).
                 07 GFCTLL-PONT-CH-DATA            PIC  X(010).
              05 GFCTLL-IND-DATA                   PIC  X(001).
      *       P-VIG/FUTURA T-TODAS
              05 GFCTLL-FIM                        PIC  X(001).
BI0810        05 GFCTLL-CPSSOA-TARIFA              PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
BI0810        05 FILLER                            PIC  X(932).
