      *****************************************************************
      * NOME DA INC - GFCTWAFK                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. DEFERIMENTO *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010 => (FIXO = 141 + 7X121=847 + FILLER 22)    *
      * WAFK-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER SILVA - PROCWORK                         *
      *****************************************************************
       01  WAFK-SAIDA.
           03 WAFK-LL                            PIC S9(004) COMP.
           03 WAFK-ZZ                            PIC S9(004) COMP.
           03 WAFK-TRANSACAO                     PIC  X(009).
           03 WAFK-FUNCAO                        PIC  X(008).
           03 WAFK-DADOS-RST.
              05 WAFK-QTDE-OCOR                  PIC  9(003).
              05 WAFK-QTDE-TOT-REG               PIC  9(006).
              05 WAFK-FUNC-BDSCO                 PIC  X(007).
              05 WAFK-ERRO                       PIC  9(001).
              05 WAFK-COD-MSG-ERRO               PIC  9(007).
              05 WAFK-COD-SQL-ERRO               PIC  9(003).
              05 WAFK-DESC-MSG-ERRO              PIC  X(079).
              05 WAFK-FILTRO.
                 07 WAFK-FLT-AGPTO               PIC  9(003).
                 07 WAFK-FLT-TARIFA              PIC  9(005).
              05 WAFK-PONTEIRO.
                 07 WAFK-PNT-TARIFA              PIC  9(005).
              05 WAFK-FIM                        PIC  X(001).
              05 FILLER                          PIC  X(022).
           03 WAFK-OCORR                         OCCURS 07 TIMES.
              05 WAFK-HINCL                      PIC  X(026).
              05 WAFK-AGENCIA                    PIC  9(005).
              05 WAFK-POSTO                      PIC  9(005).
              05 WAFK-NOME-POSTO                 PIC  X(020).
              05 WAFK-CONTA                      PIC  9(013).
              05 WAFK-CNPJ.
ST2506*          10 WAFK-CNPJ-NUM                PIC  9(009).
ST2506*          10 WAFK-CNPJ-FIL                PIC  9(005).
ST2506           10 WAFK-CNPJ-CGC                PIC  X(009).
ST2506           10 WAFK-CNPJ-FIL                PIC  X(004).
                 10 WAFK-CNPJ-CTR                PIC  9(002).
              05 WAFK-DATA-SOLIC                 PIC  X(010).
              05 WAFK-PACOTE                     PIC  9(005).
              05 WAFK-NOME-PACOTE                PIC  X(020).
              05 WAFK-ERRO-OCOR                  PIC  9(001).
