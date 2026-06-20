      *****************************************************************
      * NOME DA INC - I#GFCTLM                                        *
      * DESCRICAO   - ADESAO COMPULSORIA - CONSULTA                   *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTLM-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTLM-SAIDA.
           03 GFCTLM-LL                            PIC S9(004) COMP.
           03 GFCTLM-ZZ                            PIC S9(004) COMP.
           03 GFCTLM-TRANSACAO                     PIC  X(009).
           03 GFCTLM-FUNCAO                        PIC  X(008).
           03 GFCTLM-DADOS-RST.
              05 GFCTLM-QTDE-OCOR                  PIC  9(003).
              05 GFCTLM-QTDE-TOT-REG               PIC  9(006).
              05 GFCTLM-FUNC-BDSCO                 PIC  X(007).
              05 GFCTLM-PONTEIRO.
                 07 GFCTLM-PONT-AGPTO              PIC  9(003).
                 07 GFCTLM-PONT-PACOTE             PIC  9(005).
                 07 GFCTLM-PONT-CH-NUM5            PIC  9(005).
                 07 GFCTLM-PONT-CH-NUM13           PIC  9(013).
ST2509           07 GFCTLM-PONT-CH-CCGC-CPF        PIC  X(009).
                 07 GFCTLM-PONT-CH-ALP             PIC  X(002).
                 07 GFCTLM-PONT-CH-DATA            PIC  X(010).
              05 GFCTLM-ERRO                       PIC  9(001).
              05 GFCTLM-COD-MSG-ERRO               PIC  9(007).
              05 GFCTLM-COD-SQL-ERRO               PIC  9(003).
              05 GFCTLM-DESC-MSG-ERRO              PIC  X(079).
              05 GFCTLM-FIM                        PIC  X(001).
              05 GFCTLM-IND-DATA                   PIC  X(001).
      *       P-VIG/FUTURA T-TODAS
BI0810        05 FILLER                            PIC  X(008).
           03 GFCTLM-OCORR                         OCCURS 05 TIMES.
              05 GFCTLM-SEQUENCIA                  PIC  9(009).
              05 GFCTLM-PACOTE                     PIC  9(005).
              05 GFCTLM-DESC-PACOTE                PIC  X(040).
ST2506*       05 GFCTLM-CPF-CNPJ                   PIC  9(009).
ST2506*       05 GFCTLM-FILIAL                     PIC  9(005).
ST2506        05 GFCTLM-CPF-CNPJ                   PIC  X(009).
ST2506        05 GFCTLM-FILIAL                     PIC  X(004).
              05 GFCTLM-CONTROLE                   PIC  9(002).
              05 GFCTLM-AGENCIA                    PIC  9(005).
              05 GFCTLM-CONTA                      PIC  9(013).
              05 GFCTLM-POSTO                      PIC  9(005).
              05 GFCTLM-SEGMENTO                   PIC  9(003).
              05 GFCTLM-MUNICIPIO                  PIC  9(007).
              05 GFCTLM-UF                         PIC  X(002).
              05 GFCTLM-DESC-NOME                  PIC  X(040).
              05 GFCTLM-DATA-INICIO                PIC  X(010).
              05 GFCTLM-DATA-FIM                   PIC  X(010).
              05 GFCTLM-CANCELADO                  PIC  X(001).
BI0810        05 GFCTLM-CPSSOA-TARIFA              PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
