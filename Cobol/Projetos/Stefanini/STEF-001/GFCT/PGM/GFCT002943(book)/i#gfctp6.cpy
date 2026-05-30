      *****************************************************************
      * NOME DA INC - I#GFCTP6                                        *
      * DESCRICAO   - SUSPENSAO DE TARIFA - INCLUSAO                  *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTP6-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTP6-ENTRADA.
           03 GFCTP6-LL                            PIC S9(004) COMP.
           03 GFCTP6-ZZ                            PIC S9(004) COMP.
           03 GFCTP6-TRANSACAO                     PIC  X(009).
           03 GFCTP6-FUNCAO                        PIC  X(008).
           03 GFCTP6-DADOS-RST.
              05 GFCTP6-FUNC-BDSCO                 PIC  X(007).
              05 GFCTP6-FIM                        PIC  X(001).
              05 GFCTP6-FILLER                     PIC  X(664).
           03 GFCTP6-FIXO.
              05 GFCTP6-CTARIFA                    PIC  9(005).
              05 GFCTP6-CAGPTO                     PIC  9(003).
              05 GFCTP6-TOTAL                      PIC  X(001).
ST2506*       05 GFCTP6-CGC-CPF                    PIC  9(009).
ST2506*       05 GFCTP6-FILIAL                     PIC  9(005).
ST2506        05 GFCTP6-CGC-CPF                    PIC  X(009).
ST2506        05 GFCTP6-FILIAL                     PIC  X(004).
              05 GFCTP6-CONTROLE                   PIC  X(002).
              05 GFCTP6-CAGENCIA                   PIC  9(005).
              05 GFCTP6-CPAB                       PIC  9(005).
              05 GFCTP6-CMUNIC                     PIC  9(005).
              05 GFCTP6-CUF                        PIC  X(002).
              05 GFCTP6-DATA-INICIO                PIC  X(010).
              05 GFCTP6-DATA-FIM                   PIC  X(010).
              05 GFCTP6-JUST                       PIC  X(255).
