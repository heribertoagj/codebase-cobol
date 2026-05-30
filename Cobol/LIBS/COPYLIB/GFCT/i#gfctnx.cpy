      *****************************************************************
      * NOME DA INC - I#GFCTNX                                        *
      * DESCRICAO   - EXCECAO DE ADESAO COMPULSORIA - CANCELAMENTO    *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
TESTE * TAMANHO     - 1010                                            *
TESTE * GFCTNX-LL   - 1010                                            *
TESTE * TAMANHO     - 0129                                            *
TESTE * GFCTNX-LL   - 0129                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTNX-ENTRADA.
           03 GFCTNX-LL                            PIC S9(004) COMP.
           03 GFCTNX-ZZ                            PIC S9(004) COMP.
           03 GFCTNX-TRANSACAO                     PIC  X(009).
           03 GFCTNX-FUNCAO                        PIC  X(008).
           03 GFCTNX-DADOS-RST.
              05 GFCTNX-FUNC-BDSCO                 PIC  X(007).
              05 GFCTNX-TERMINAL                   PIC  X(008).
              05 GFCTNX-FIM                        PIC  X(001).
              05 GFCTNX-FILLER                     PIC  X(001).
           03 GFCTNX-FIXO.
              05 GFCTNX-CAGENCIA-EXCEC             PIC  9(005).
              05 GFCTNX-CCONTA-EXCEC               PIC  9(013).
              05 GFCTNX-CPACOTE                    PIC  9(005).
              05 GFCTNX-CAGPTO                     PIC  9(003).
              05 GFCTNX-DINI-ADESAO                PIC  X(010).
              05 GFCTNX-SEQUENCIA                  PIC  9(009).
              05 GFCTNX-HINCL                      PIC  X(026).
              05 GFCTNX-DATA-INICIO-APOS           PIC  X(010).
              05 GFCTNX-DATA-FIM-APOS              PIC  X(010).
