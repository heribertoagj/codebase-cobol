      *****************************************************************
      * NOME DA INC - GFCTWAFQ                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. CONSULTA    *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * WAFG-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER  - PROCWORK                              *
      *****************************************************************
      *
       01  WAFQ-SAIDA.
           03  WAFQ-LL                            PIC S9(004) COMP.
           03  WAFQ-ZZ                            PIC S9(004) COMP.
           03  WAFQ-TRANSACAO                     PIC  X(009).
           03  WAFQ-FUNCAO                        PIC  X(008).
           03  WAFQ-DADOS-REST.
               05  WAFQ-FUNC-BDSCO                PIC  X(007).
               05  WAFQ-TERMINAL                  PIC  X(008).
               05  WAFQ-FIM                       PIC  X(001).
           03  WAFQ-ERRO                          PIC  9(001).
           03  WAFQ-COD-MSG-ERRO                  PIC  9(007).
           03  WAFQ-COD-SQL-ERRO                  PIC  9(003).
           03  WAFQ-DESC-MSG-ERRO                 PIC  X(079).
           03  WAFQ-FILLER                        PIC  X(002).
           03  WAFQ-FIXO.
               05 WAFQ-HINCL                      PIC  X(026).
               05 WAFQ-CAGPTO                     PIC  9(003).
               05 WAFQ-RAGPTO                     PIC  X(030).
               05 WAFQ-CPACOTE                    PIC  9(005).
               05 WAFQ-RPACOTE                    PIC  X(035).
               05 WAFQ-CAGENCIA                   PIC  9(005).
               05 WAFQ-RAGENCIA                   PIC  X(030).
               05 WAFQ-CPOSTO                     PIC  9(005).
               05 WAFQ-RPOSTO                     PIC  X(030).
               05 WAFQ-CCONTA                     PIC  9(013).
               05 WAFQ-RCONTA                     PIC  X(035).
               05 WAFQ-CNPJ.
ST2506*           10 WAFQ-CNPJ-NUM                PIC  9(009).
ST2506*           10 WAFQ-CNPJ-FIL                PIC  9(005).
ST2506            10 WAFQ-CNPJ-CGC                PIC  X(009).
ST2506            10 WAFQ-CNPJ-FIL                PIC  X(004).
                  10 WAFQ-CNPJ-CTR                PIC  9(002).
               05 WAFQ-RCNPJ                      PIC  X(035).
               05 WAFQ-DATA-SOLIC                 PIC  X(010).
               05 WAFQ-COMENT                     PIC  X(235).
               05 WAFQ-CFUNC-SOLIC                PIC  X(007).
               05 WAFQ-RFUNC-SOLIC                PIC  X(030).
               05 WAFQ-DATA-INCL                  PIC  X(010).
               05 WAFQ-HORA-INCL                  PIC  X(005).
               05 WAFQ-QTD-PREVIA                 PIC  9(013).
               05 WAFQ-QTD-EXCLUIDAS              PIC  9(013).
               05 WAFQ-CFUNC-CANC-PAREC           PIC  X(007).
               05 WAFQ-RFUNC-CANC-PAREC           PIC  X(030).
               05 WAFQ-DATA-CANC-PAREC            PIC  X(010).
               05 WAFQ-HORA-CANC-PAREC            PIC  X(005).
               05 WAFQ-SITUACAO                   PIC  9(002).
               05 WAFQ-PARECER                    PIC  9(001).
      *           1 = DEFERIDO / 2 = INDEFERIDO
               05 WAFQ-JUSTIF                     PIC  X(235).
