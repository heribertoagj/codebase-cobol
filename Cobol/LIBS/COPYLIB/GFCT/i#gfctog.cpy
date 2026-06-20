      *****************************************************************
      * NOME DA INC - I#GFCTOG                                        *
      * DESCRICAO   - EXCECAO DE ADESAO COMPULSORIA - CONS DO CANCEL  *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTOG-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTOG-SAIDA.
           03 GFCTOG-LL                            PIC S9(004) COMP.
           03 GFCTOG-ZZ                            PIC S9(004) COMP.
           03 GFCTOG-TRANSACAO                     PIC  X(009).
           03 GFCTOG-FUNCAO                        PIC  X(008).
           03 GFCTOG-DADOS-RST.
              05 GFCTOG-FUNC-BDSCO                 PIC  X(007).
              05 GFCTOG-TERMINAL                   PIC  X(008).
              05 GFCTOG-FIM                        PIC  X(001).
           03 GFCTOG-ERRO                          PIC  9(001).
           03 GFCTOG-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTOG-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTOG-DESC-MSG-ERRO                 PIC  X(079).
           03 GFCTOG-FILLER                        PIC  X(521).
           03 GFCTOG-FIXO.
              05 GFCTOG-CAGENCIA-EXCEC             PIC  9(005).
              05 GFCTOG-CCONTA-EXCEC               PIC  9(013).
              05 GFCTOG-CPACOTE                    PIC  9(005).
              05 GFCTOG-CAGPTO                     PIC  9(003).
              05 GFCTOG-DINI-ADESAO                PIC  X(010).
              05 GFCTOG-SEQUENCIA                  PIC  9(009).
              05 GFCTOG-HINCL                      PIC  X(026).

ST2506*       05 GFCTOG-CGC-CPF                    PIC  9(009).
ST2506*       05 GFCTOG-FILIAL                     PIC  9(005).
ST2506        05 GFCTOG-CGC-CPF                    PIC  X(009).
ST2506        05 GFCTOG-FILIAL                     PIC  X(004).
              05 GFCTOG-CONTROLE                   PIC  X(002).
              05 GFCTOG-CAGENCIA                   PIC  9(005).
              05 GFCTOG-RAGENCIA                   PIC  X(030).
              05 GFCTOG-CCONTA                     PIC  9(013).
              05 GFCTOG-RCONTA                     PIC  X(050).
              05 GFCTOG-CPAB                       PIC  9(005).
              05 GFCTOG-RPAB                       PIC  X(030).
              05 GFCTOG-TOTAL                      PIC  X(001).
              05 GFCTOG-CSEGMENTO                  PIC  9(003).
              05 GFCTOG-RSEGMENTO                  PIC  X(030).
              05 GFCTOG-CMUNIC                     PIC  9(005).
              05 GFCTOG-RMUNIC                     PIC  X(030).
              05 GFCTOG-CUF                        PIC  X(002).
              05 GFCTOG-RUF                        PIC  X(030).
              05 GFCTOG-DATA-INICIO-ANTES          PIC  X(010).
              05 GFCTOG-DATA-FIM-ANTES             PIC  X(010).
              05 GFCTOG-DATA-INICIO-APOS           PIC  X(010).
              05 GFCTOG-DATA-FIM-APOS              PIC  X(010).
