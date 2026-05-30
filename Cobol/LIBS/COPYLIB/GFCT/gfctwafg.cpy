      *****************************************************************
      * NOME DA INC - GFCTWAFG                                        *
      * DESCRICAO   - EXCLUSAO MASSIVA ADESAO CESTA SERVICOS INCLUSAO *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * WAFG-LL     - 1010                                            *
      * DATA        - FEVEREIRO/2009                                  *
      * RESPONSAVEL - WAGNER  - PROCWORK                              *
      *****************************************************************
      *
       01  WAFG-SAIDA.
           03 WAFG-LL                            PIC S9(004) COMP.
           03 WAFG-ZZ                            PIC S9(004) COMP.
           03 WAFG-TRANSACAO                     PIC  X(009).
           03 WAFG-FUNCAO                        PIC  X(008).
           03 WAFG-DADOS-RST.
              05 WAFG-FUNC-BDSCO                 PIC  X(007).
              05 WAFG-TERMINAL                   PIC  X(008).
              05 WAFG-FIM                        PIC  X(001).
           03 WAFG-ERRO                          PIC  9(001).
           03 WAFG-COD-MSG-ERRO                  PIC  9(007).
           03 WAFG-COD-SQL-ERRO                  PIC  9(003).
           03 WAFG-DESC-MSG-ERRO                 PIC  X(079).
           03 WAFG-FILLER                        PIC  X(556).
           03 WAFG-FIXO.
              05 WAFG-CAGPTO                     PIC  9(003).
              05 WAFG-CTARIF                     PIC  9(005).
              05 WAFG-HINCL                      PIC  X(026).
              05 WAFG-CAGEN                      PIC  9(005).
              05 WAFG-CPAB                       PIC  9(005).
              05 WAFG-CCTA                       PIC  9(013).
              05 WAFG-CCPF.
                 10 WAFG-CCPF-NUM                PIC  9(009).
                 10 WAFG-CCPF-FIL                PIC  9(005).
                 10 WAFG-CCPF-CTR                PIC  9(002).
              05 WAFG-JUSTIF                     PIC  X(254).
