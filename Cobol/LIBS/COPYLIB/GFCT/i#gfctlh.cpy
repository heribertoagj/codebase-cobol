      *****************************************************************
      * NOME DA INC - I#GFCTLH                                        *
      * DESCRICAO   - ADESAO COMPULSORIA - INCLUSAO                   *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTLH-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTLH-ENTRADA.
           03 GFCTLH-LL                            PIC S9(004) COMP.
           03 GFCTLH-ZZ                            PIC S9(004) COMP.
           03 GFCTLH-TRANSACAO                     PIC  X(009).
           03 GFCTLH-FUNCAO                        PIC  X(008).
           03 GFCTLH-DADOS-RST.
              05 GFCTLH-FUNC-BDSCO                 PIC  X(007).
              05 GFCTLH-TERMINAL                   PIC  X(008).
              05 GFCTLH-FIM                        PIC  X(001).
BI0810        05 GFCTLH-FILLER                     PIC  X(614).
           03 GFCTLH-FIXO.
              05 GFCTLH-CAGPTO                     PIC  9(003).
              05 GFCTLH-RAGPTO                     PIC  X(030).
              05 GFCTLH-TOTAL                      PIC  X(001).
ST2506*       05 GFCTLH-CGC-CPF                    PIC  9(009).
ST2506*       05 GFCTLH-FILIAL                     PIC  9(005).
ST2506        05 GFCTLH-CGC-CPF                    PIC  X(009).
ST2506        05 GFCTLH-FILIAL                     PIC  X(005).
              05 GFCTLH-CONTROLE                   PIC  X(002).
              05 GFCTLH-CAGENCIA                   PIC  9(005).
              05 GFCTLH-RAGENCIA                   PIC  X(030).
              05 GFCTLH-CCONTA                     PIC  9(013).
              05 GFCTLH-RCONTA                     PIC  X(050).
              05 GFCTLH-CPAB                       PIC  9(005).
              05 GFCTLH-RPAB                       PIC  X(030).
              05 GFCTLH-CSEGMENTO                  PIC  9(003).
              05 GFCTLH-RSEGMENTO                  PIC  X(030).
              05 GFCTLH-CMUNIC                     PIC  9(005).
              05 GFCTLH-RMUNIC                     PIC  X(030).
              05 GFCTLH-CUF                        PIC  X(002).
              05 GFCTLH-RUF                        PIC  X(030).
              05 GFCTLH-CPACOTE                    PIC  9(005).
              05 GFCTLH-RPACOTE                    PIC  X(030).
              05 GFCTLH-DATA-INICIO-PCTE           PIC  X(010).
              05 GFCTLH-DATA-FIM-PCTE              PIC  X(010).
              05 GFCTLH-DATA-INICIO                PIC  X(010).
              05 GFCTLH-DATA-FIM                   PIC  X(010).
BI0810        05 GFCTLH-CPSSOA-TARIFA              PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
