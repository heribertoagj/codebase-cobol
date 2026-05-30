      *****************************************************************
      * NOME DA INC - GFCTWAFP                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. CONSULTA    *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * WAFG-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER  - PROCWORK                              *
      *****************************************************************
      *
       01  WAFP-ENTRADA.
           03  WAFP-LL                            PIC S9(004) COMP.
           03  WAFP-ZZ                            PIC S9(004) COMP.
           03  WAFP-TRANSACAO                     PIC  X(009).
           03  WAFP-FUNCAO                        PIC  X(008).
           03  WAFP-DADOS-REST.
               05  WAFP-FUNC-BDSCO                PIC  X(007).
               05  WAFP-TERMINAL                  PIC  X(008).
               05  WAFP-FIM                       PIC  X(001).
           03  WAFP-FILLER                        PIC  X(092).
           03  WAFP-FIXO.
               05 WAFP-HINCL                      PIC  X(026).
               05 WAFP-CAGPTO                     PIC  9(003).
               05 WAFP-RAGPTO                     PIC  X(030).
               05 WAFP-CPACOTE                    PIC  9(005).
               05 WAFP-RPACOTE                    PIC  X(035).
               05 WAFP-CAGENCIA                   PIC  9(005).
               05 WAFP-RAGENCIA                   PIC  X(030).
               05 WAFP-CPOSTO                     PIC  9(005).
               05 WAFP-RPOSTO                     PIC  X(030).
               05 WAFP-CCONTA                     PIC  9(013).
               05 WAFP-RCONTA                     PIC  X(035).
               05 WAFP-CNPJ.
ST2506*           10 WAFP-CNPJ-NUM                PIC  9(009).
ST2506*           10 WAFP-CNPJ-FIL                PIC  9(005).
ST2506            10 WAFP-CNPJ-CGC                PIC  X(009).
ST2506            10 WAFP-CNPJ-FIL                PIC  X(004).
                  10 WAFP-CNPJ-CTR                PIC  9(002).
               05 WAFP-RCNPJ                      PIC  X(035).
               05 WAFP-DATA-SOLIC                 PIC  X(010).
               05 WAFP-COMENT                     PIC  X(235).
               05 WAFP-CFUNC-SOLIC                PIC  X(007).
               05 WAFP-RFUNC-SOLIC                PIC  X(030).
               05 WAFP-DATA-INCL                  PIC  X(010).
               05 WAFP-HORA-INCL                  PIC  X(005).
               05 WAFP-QTD-PREVIA                 PIC  9(013).
               05 WAFP-QTD-EXCLUIDAS              PIC  9(013).
               05 WAFP-CFUNC-CANC-PAREC           PIC  X(007).
               05 WAFP-RFUNC-CANC-PAREC           PIC  X(030).
               05 WAFP-DATA-CANC-PAREC            PIC  X(010).
               05 WAFP-HORA-CANC-PAREC            PIC  X(005).
               05 WAFP-SITUACAO                   PIC  9(002).
               05 WAFP-PARECER                    PIC  9(001).
      *           1 = DEFERIDO / 2 = INDEFERIDO
               05 WAFP-JUSTIF                     PIC  X(235).
