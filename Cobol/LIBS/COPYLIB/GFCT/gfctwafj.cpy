      *****************************************************************
      * NOME DA INC - GFCTWAFJ                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. DEFERIMENTO *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010 => (FIXO = 046 + 7X121=847 + FILLER 117)   *
      * WAFK-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER SILVA - PROCWORK                         *
      *****************************************************************
      *
       01  WAFJ-ENTRADA.
           03 WAFJ-LL                            PIC S9(004) COMP.
           03 WAFJ-ZZ                            PIC S9(004) COMP.
           03 WAFJ-TRANSACAO                     PIC  X(009).
           03 WAFJ-FUNCAO                        PIC  X(008).
           03 WAFJ-DADOS-RST.
              05 WAFJ-QTDE-OCOR                  PIC  9(003).
              05 WAFJ-QTDE-TOT-REG               PIC  9(006).
              05 WAFJ-FUNC-BDSCO                 PIC  X(007).
              05 WAFJ-FILTRO.
                 07 WAFJ-FLT-AGPTO               PIC  9(003).
                 07 WAFJ-FLT-TARIFA              PIC  9(005).
              05 WAFJ-PONTEIRO.
                 07 WAFJ-PNT-TARIFA              PIC  9(005).
              05 WAFJ-FIM                        PIC  X(001).
              05 FILLER                          PIC  X(112).
           03 WAFJ-OCORR                         OCCURS 07 TIMES.
              05 WAFJ-HINCL                      PIC  X(026).
              05 WAFJ-AGENCIA                    PIC  9(005).
              05 WAFJ-POSTO                      PIC  9(005).
              05 WAFJ-NOME-POSTO                 PIC  X(020).
              05 WAFJ-CONTA                      PIC  9(013).
              05 WAFJ-CNPJ.
                 10 WAFJ-CNPJ-NUM                PIC  9(009).
                 10 WAFJ-CNPJ-FIL                PIC  9(005).
                 10 WAFJ-CNPJ-CTR                PIC  9(002).
              05 WAFJ-DATA-SOLIC                 PIC  X(010).
              05 WAFJ-PACOTE                     PIC  9(005).
              05 WAFJ-NOME-PACOTE                PIC  X(020).
              05 WAFJ-ERRO-OCOR                  PIC  9(001).
