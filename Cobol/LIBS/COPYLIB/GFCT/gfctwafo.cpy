      *****************************************************************
      * NOME DA INC - GFCTWAFO                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. CONSULTA    *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010 => (FIXO = 136 + 7X123=861 + FILLER 13)    *
      * WAFO-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER SILVA - PROCWORK                         *
      *****************************************************************
       01  WAFO-SAIDA.
           03 WAFO-LL                            PIC S9(004) COMP.
           03 WAFO-ZZ                            PIC S9(004) COMP.
           03 WAFO-TRANSACAO                     PIC  X(009).
           03 WAFO-FUNCAO                        PIC  X(008).
           03 WAFO-DADOS-RST.
              05 WAFO-QTDE-OCOR                  PIC  9(003).
              05 WAFO-QTDE-TOT-REG               PIC  9(006).
              05 WAFO-FUNC-BDSCO                 PIC  X(007).
              05 WAFO-ERRO                       PIC  9(001).
              05 WAFO-COD-MSG-ERRO               PIC  9(007).
              05 WAFO-COD-SQL-ERRO               PIC  9(003).
              05 WAFO-DESC-MSG-ERRO              PIC  X(079).
              05 WAFO-AGPTO                      PIC  9(003).
              05 WAFO-COD-PACOTE                 PIC  9(005).
              05 WAFO-FIM                        PIC  X(001).
              05 FILLER                          PIC  X(010).
           03 WAFO-OCORR                         OCCURS 07 TIMES.
              05 WAFO-HINCL                      PIC  X(026).
              05 WAFO-AGENCIA                    PIC  9(005).
              05 WAFO-POSTO                      PIC  9(005).
              05 WAFO-NOME-POSTO                 PIC  X(020).
              05 WAFO-CONTA                      PIC  9(013).
              05 WAFO-CNPJ.
ST2506*          10 WAFO-CNPJ-NUM                PIC  9(009).
ST2506*          10 WAFO-CNPJ-FIL                PIC  9(005).
ST2506           10 WAFO-CNPJ-CGC                PIC  X(009).
ST2506           10 WAFO-CNPJ-FIL                PIC  X(004).
                 10 WAFO-CNPJ-CTR                PIC  9(002).
              05 WAFO-DATA-SOLIC                 PIC  X(010).
              05 WAFO-PACOTE                     PIC  9(005).
              05 WAFO-NOME-PACOTE                PIC  X(020).
              05 WAFO-SITUACAO                   PIC  9(002).
              05 WAFO-ERRO-OCOR                  PIC  9(001).
