      *****************************************************************
      * NOME DA INC - GFCTWAFI                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERV. CANCELAMENTO*
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010 => (FIXO = 136 + 5X123=615 + FILLER 259)   *
      * WAFI-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER SILVA - PROCWORK                         *
      *****************************************************************
      *
       01  WAFI-SAIDA.
           03 WAFI-LL                            PIC S9(004) COMP.
           03 WAFI-ZZ                            PIC S9(004) COMP.
           03 WAFI-TRANSACAO                     PIC  X(009).
           03 WAFI-FUNCAO                        PIC  X(008).
           03 WAFI-DADOS-RST.
              05 WAFI-QTDE-OCOR                  PIC  9(003).
              05 WAFI-QTDE-TOT-REG               PIC  9(006).
              05 WAFI-FUNC-BDSCO                 PIC  X(007).
              05 WAFI-ERRO                       PIC  9(001).
              05 WAFI-COD-MSG-ERRO               PIC  9(007).
              05 WAFI-COD-SQL-ERRO               PIC  9(003).
              05 WAFI-DESC-MSG-ERRO              PIC  X(079).
              05 WAFI-AGPTO                      PIC  9(003).
              05 WAFI-COD-PACOTE                 PIC  9(005).
              05 WAFI-FIM                        PIC  X(001).
              05 WAFI-OPCAO                      PIC  9(001).
      *          1 - CONSULTA    2 - EFETIVACAO
              05 FILLER                          PIC  X(258).
           03 WAFI-OCORR                         OCCURS 05 TIMES.
              05 WAFI-HINCL                      PIC  X(026).
              05 WAFI-AGENCIA                    PIC  9(005).
              05 WAFI-POSTO                      PIC  9(005).
              05 WAFI-NOME-POSTO                 PIC  X(020).
              05 WAFI-CONTA                      PIC  9(013).
              05 WAFI-CNPJ.
ST2506*          10 WAFI-CNPJ-NUM                PIC  9(009).
ST2506*          10 WAFI-CNPJ-FIL                PIC  9(005).
ST2506           10 WAFI-CNPJ-CGC                PIC  X(009).
ST2506           10 WAFI-CNPJ-FIL                PIC  X(004).
                 10 WAFI-CNPJ-CTR                PIC  9(002).
              05 WAFI-DATA-SOLIC                 PIC  X(010).
              05 WAFI-PACOTE                     PIC  9(005).
              05 WAFI-NOME-PACOTE                PIC  X(020).
              05 WAFI-SITUACAO                   PIC  9(002).
              05 WAFI-ERRO-OCOR                  PIC  9(001).
