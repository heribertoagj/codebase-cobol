      *****************************************************************
      * NOME DA INC - I#GFCTNY                                        *
      * DESCRICAO   - EXCECAO ADESAO COMPULSORIA - CANCELAMENTO       *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
TESTE * TAMANHO     - 1010                                            *
TESTE * GFCTNY-LL   - 1010                                            *
TESTE * TAMANHO     - 0219                                            *
TESTE * GFCTNY-LL   - 0219                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTNY-SAIDA.
           03 GFCTNY-LL                            PIC S9(004) COMP.
           03 GFCTNY-ZZ                            PIC S9(004) COMP.
           03 GFCTNY-TRANSACAO                     PIC  X(009).
           03 GFCTNY-FUNCAO                        PIC  X(008).
           03 GFCTNY-DADOS-RST.
              05 GFCTNY-FUNC-BDSCO                 PIC  X(007).
              05 GFCTNY-TERMINAL                   PIC  X(008).
              05 GFCTNY-FIM                        PIC  X(001).
           03 GFCTNY-ERRO                          PIC  9(001).
           03 GFCTNY-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTNY-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTNY-DESC-MSG-ERRO                 PIC  X(079).
           03 GFCTNY-FILLER                        PIC  X(001).
           03 GFCTNY-FIXO.
              05 GFCTNY-CAGENCIA-EXCEC             PIC  9(005).
              05 GFCTNY-CCONTA-EXCEC               PIC  9(013).
              05 GFCTNY-CPACOTE                    PIC  9(005).
              05 GFCTNY-CAGPTO                     PIC  9(003).
              05 GFCTNY-DINI-ADESAO                PIC  X(010).
              05 GFCTNY-SEQUENCIA                  PIC  9(009).
              05 GFCTNY-HINCL                      PIC  X(026).
              05 GFCTNY-DATA-INICIO-APOS           PIC  X(010).
              05 GFCTNY-DATA-FIM-APOS              PIC  X(010).
