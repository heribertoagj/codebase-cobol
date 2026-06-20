      *****************************************************************
      * NOME DA INC - GFCTWAFM                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. DEFERIMENTO *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * WAFG-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER  - PROCWORK                              *
      *****************************************************************
      *
       01  WAFM-SAIDA.
           03  WAFM-LL                            PIC S9(004) COMP.
           03  WAFM-ZZ                            PIC S9(004) COMP.
           03  WAFM-TRANSACAO                     PIC  X(009).
           03  WAFM-FUNCAO                        PIC  X(008).
           03  WAFM-DADOS-REST.
               05  WAFM-FUNC-BDSCO                PIC  X(007).
               05  WAFM-TERMINAL                  PIC  X(008).
               05  WAFM-FIM                       PIC  X(001).
           03  WAFM-ERRO                          PIC  9(001).
           03  WAFM-COD-MSG-ERRO                  PIC  9(007).
           03  WAFM-COD-SQL-ERRO                  PIC  9(003).
           03  WAFM-DESC-MSG-ERRO                 PIC  X(079).
           03  WAFM-FILLER                        PIC  X(013).
           03 WAFM-FIXO.
               05 WAFM-OPCAO                      PIC  X(001).
      *           1 = CONSULTA  /  2 = EFETIVACAO
               05 WAFM-HINCL                      PIC  X(026).
               05 WAFM-CAGPTO                     PIC  9(003).
               05 WAFM-RAGPTO                     PIC  X(030).
               05 WAFM-CPACOTE                    PIC  9(005).
               05 WAFM-RPACOTE                    PIC  X(040).
               05 WAFM-CAGENCIA                   PIC  9(005).
               05 WAFM-RAGENCIA                   PIC  X(030).
               05 WAFM-CPOSTO                     PIC  9(005).
               05 WAFM-RPOSTO                     PIC  X(030).
               05 WAFM-CCONTA                     PIC  9(013).
               05 WAFM-RCONTA                     PIC  X(040).
               05 WAFM-CNPJ.
ST2506*           10 WAFM-CNPJ-NUM                PIC  9(009).
ST2506*           10 WAFM-CNPJ-FIL                PIC  9(005).
ST2506            10 WAFM-CNPJ-CGC                PIC  X(009).
ST2506            10 WAFM-CNPJ-FIL                PIC  X(004).
                  10 WAFM-CNPJ-CTR                PIC  9(002).
               05 WAFM-RCNPJ                      PIC  X(040).
               05 WAFM-DATA-SOLIC                 PIC  X(010).
               05 WAFM-COMENT                     PIC  X(255).
               05 WAFM-CFUNC-SOLIC                PIC  X(007).
               05 WAFM-RFUNC-SOLIC                PIC  X(030).
               05 WAFM-DATA-INCL                  PIC  X(010).
               05 WAFM-HORA-INCL                  PIC  X(005).
               05 WAFM-QTD-PREVIA                 PIC  9(013).
               05 WAFM-PARECER                    PIC  9(001).
      *           1 = DEFERIDO / 2 = INDEFERIDO
               05 WAFM-JUSTIF                     PIC  X(255).
