      *****************************************************************
      * NOME DA INC - GFCTWAFN                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. CONSULTA    *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010 => (FIXO = 046 + 7X123=861 + FILLER 103)   *
      * WAFK-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER SILVA - PROCWORK                         *
      *================================================================*
ST2509*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2509*    ANALISTA....:  STEFANINI                                    *
ST2509*    DATA........:  09 / 2025                                    *
ST2509*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  WAFN-ENTRADA.
           03 WAFN-LL                            PIC S9(004) COMP.
           03 WAFN-ZZ                            PIC S9(004) COMP.
           03 WAFN-TRANSACAO                     PIC  X(009).
           03 WAFN-FUNCAO                        PIC  X(008).
           03 WAFN-DADOS-RST.
              05 WAFN-QTDE-OCOR                  PIC  9(003).
              05 WAFN-QTDE-TOT-REG               PIC  9(006).
              05 WAFN-FUNC-BDSCO                 PIC  X(007).
              05 WAFN-AGPTO                      PIC  9(003).
              05 WAFN-COD-PACOTE                 PIC  9(005).
              05 WAFN-FIM                        PIC  X(001).
              05 FILLER                          PIC  X(103).
           03 WAFN-OCORR                         OCCURS 07 TIMES.
              05 WAFN-HINCL                      PIC  X(026).
              05 WAFN-AGENCIA                    PIC  9(005).
              05 WAFN-POSTO                      PIC  9(005).
              05 WAFN-NOME-POSTO                 PIC  X(020).
              05 WAFN-CONTA                      PIC  9(013).
              05 WAFN-CNPJ.
ST2509           10 WAFN-CNPJ-NUM                PIC  X(009).
ST2509           10 WAFN-CNPJ-FIL                PIC  X(005).
                 10 WAFN-CNPJ-CTR                PIC  9(002).
              05 WAFN-DATA-SOLIC                 PIC  X(010).
              05 WAFN-PACOTE                     PIC  9(005).
              05 WAFN-NOME-PACOTE                PIC  X(020).
              05 WAFN-SITUACAO                   PIC  9(002).
              05 WAFN-ERRO-OCOR                  PIC  9(001).
