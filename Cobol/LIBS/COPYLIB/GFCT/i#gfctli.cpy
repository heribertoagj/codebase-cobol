      *****************************************************************
      * NOME DA INC - I#GFCTLI                                        *
      * DESCRICAO   - ADESAO COMPULSORIA - INCLUSAO                   *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTLI-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTLI-SAIDA.
           03 GFCTLI-LL                            PIC S9(004) COMP.
           03 GFCTLI-ZZ                            PIC S9(004) COMP.
           03 GFCTLI-TRANSACAO                     PIC  X(009).
           03 GFCTLI-FUNCAO                        PIC  X(008).
           03 GFCTLI-DADOS-RST.
              05 GFCTLI-FUNC-BDSCO                 PIC  X(007).
              05 GFCTLI-TERMINAL                   PIC  X(008).
              05 GFCTLI-FIM                        PIC  X(001).
           03 GFCTLI-ERRO                          PIC  9(001).
           03 GFCTLI-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTLI-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTLI-DESC-MSG-ERRO                 PIC  X(079).
BI0810     03 GFCTLI-FILLER                        PIC  X(522).
           03 GFCTLI-FIXO.
              05 GFCTLI-CAGPTO                     PIC  9(003).
              05 GFCTLI-RAGPTO                     PIC  X(030).
              05 GFCTLI-TOTAL                      PIC  X(001).
              05 GFCTLI-CGC-CPF                    PIC  9(009).
              05 GFCTLI-FILIAL                     PIC  9(005).
              05 GFCTLI-CONTROLE                   PIC  X(002).
              05 GFCTLI-CAGENCIA                   PIC  9(005).
              05 GFCTLI-RAGENCIA                   PIC  X(030).
              05 GFCTLI-CCONTA                     PIC  9(013).
              05 GFCTLI-RCONTA                     PIC  X(050).
              05 GFCTLI-CPAB                       PIC  9(005).
              05 GFCTLI-RPAB                       PIC  X(030).
              05 GFCTLI-CSEGMENTO                  PIC  9(003).
              05 GFCTLI-RSEGMENTO                  PIC  X(030).
              05 GFCTLI-CMUNIC                     PIC  9(005).
              05 GFCTLI-RMUNIC                     PIC  X(030).
              05 GFCTLI-CUF                        PIC  X(002).
              05 GFCTLI-RUF                        PIC  X(030).
              05 GFCTLI-CPACOTE                    PIC  9(005).
              05 GFCTLI-RPACOTE                    PIC  X(030).
              05 GFCTLI-DATA-INICIO-PCTE           PIC  X(010).
              05 GFCTLI-DATA-FIM-PCTE              PIC  X(010).
              05 GFCTLI-DATA-INICIO                PIC  X(010).
              05 GFCTLI-DATA-FIM                   PIC  X(010).
BI0810        05 GFCTLI-CPSSOA-TARIFA              PIC  X(001).
BI0810*          '1' - FISICA
BI0810*          '2' - JURIDICA
BI0810*          '3' - AMBAS
