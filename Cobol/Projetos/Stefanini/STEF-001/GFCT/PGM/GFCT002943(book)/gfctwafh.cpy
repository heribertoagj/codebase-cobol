      *****************************************************************
      * NOME DA INC - GFCTWAFH                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. CANCELAMENTO*
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010 => (FIXO = 046 + 5X123=615 + FILLER 349)   *
      * WAFK-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER SILVA - PROCWORK                         *
      *****************************************************************
      *
       01  WAFH-ENTRADA.
           03 WAFH-LL                            PIC S9(004) COMP.
           03 WAFH-ZZ                            PIC S9(004) COMP.
           03 WAFH-TRANSACAO                     PIC  X(009).
           03 WAFH-FUNCAO                        PIC  X(008).
           03 WAFH-DADOS-RST.
              05 WAFH-QTDE-OCOR                  PIC  9(003).
              05 WAFH-QTDE-TOT-REG               PIC  9(006).
              05 WAFH-FUNC-BDSCO                 PIC  X(007).
              05 WAFH-AGPTO                      PIC  9(003).
              05 WAFH-COD-PACOTE                 PIC  9(005).
              05 WAFH-FIM                        PIC  X(001).
              05 WAFH-OPCAO                      PIC  9(001).
      *          1 - CONSULTA    2 - EFETIVACAO
              05 FILLER                          PIC  X(348).
           03 WAFH-OCORR                         OCCURS 05 TIMES.
              05 WAFH-HINCL                      PIC  X(026).
              05 WAFH-AGENCIA                    PIC  9(005).
              05 WAFH-POSTO                      PIC  9(005).
              05 WAFH-NOME-POSTO                 PIC  X(020).
              05 WAFH-CONTA                      PIC  9(013).
              05 WAFH-CNPJ.
ST2506*          10 WAFH-CNPJ-NUM                PIC  9(009).
ST2506*          10 WAFH-CNPJ-FIL                PIC  9(005).
ST2506           10 WAFH-CNPJ-CGC                PIC  X(009).
ST2506           10 WAFH-CNPJ-FIL                PIC  X(004).
                 10 WAFH-CNPJ-CTR                PIC  9(002).
              05 WAFH-DATA-SOLIC                 PIC  X(010).
              05 WAFH-PACOTE                     PIC  9(005).
              05 WAFH-NOME-PACOTE                PIC  X(020).
              05 WAFH-SITUACAO                   PIC  9(002).
              05 WAFH-ERRO-OCOR                  PIC  9(001).
