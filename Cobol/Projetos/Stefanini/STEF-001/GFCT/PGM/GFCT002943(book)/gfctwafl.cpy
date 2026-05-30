      *****************************************************************
      * NOME DA INC - GFCTWAFL                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. DEFERIMENTO *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * WAFG-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER  - PROCWORK                              *
      *****************************************************************
      *
       01  WAFL-ENTRADA.
           03  WAFL-LL                            PIC S9(004) COMP.
           03  WAFL-ZZ                            PIC S9(004) COMP.
           03  WAFL-TRANSACAO                     PIC  X(009).
           03  WAFL-FUNCAO                        PIC  X(008).
           03  WAFL-DADOS-REST.
               05  WAFL-FUNC-BDSCO                PIC  X(007).
               05  WAFL-TERMINAL                  PIC  X(008).
               05  WAFL-FIM                       PIC  X(001).
           03  WAFL-FILLER                        PIC  X(103).
           03 WAFL-FIXO.
               05 WAFL-OPCAO                      PIC  X(001).
      *           1 = CONSULTA  /  2 = EFETIVACAO
               05 WAFL-HINCL                      PIC  X(026).
               05 WAFL-CAGPTO                     PIC  9(003).
               05 WAFL-RAGPTO                     PIC  X(030).
               05 WAFL-CPACOTE                    PIC  9(005).
               05 WAFL-RPACOTE                    PIC  X(040).
               05 WAFL-CAGENCIA                   PIC  9(005).
               05 WAFL-RAGENCIA                   PIC  X(030).
               05 WAFL-CPOSTO                     PIC  9(005).
               05 WAFL-RPOSTO                     PIC  X(030).
               05 WAFL-CCONTA                     PIC  9(013).
               05 WAFL-RCONTA                     PIC  X(040).
               05 WAFL-CNPJ.
ST2506*           10 WAFL-CNPJ-NUM                PIC  9(009).
ST2506*           10 WAFL-CNPJ-FIL                PIC  9(005).
ST2506            10 WAFL-CNPJ-CGC                PIC  X(009).
ST2506            10 WAFL-CNPJ-FIL                PIC  X(004).
                  10 WAFL-CNPJ-CTR                PIC  9(002).
               05 WAFL-RCNPJ                      PIC  X(040).
               05 WAFL-DATA-SOLIC                 PIC  X(010).
               05 WAFL-COMENT                     PIC  X(255).
               05 WAFL-CFUNC-SOLIC                PIC  X(007).
               05 WAFL-RFUNC-SOLIC                PIC  X(030).
               05 WAFL-DATA-INCL                  PIC  X(010).
               05 WAFL-HORA-INCL                  PIC  X(005).
               05 WAFL-QTD-PREVIA                 PIC  9(013).
               05 WAFL-PARECER                    PIC  9(001).
      *           1 = DEFERIDO / 2 = INDEFERIDO
               05 WAFL-JUSTIF                     PIC  X(255).
