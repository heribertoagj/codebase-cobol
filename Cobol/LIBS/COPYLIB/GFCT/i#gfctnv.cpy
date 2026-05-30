      *****************************************************************
      * NOME DA INC - I#GFCTNV                                        *
      * DESCRICAO   - EXCECAO ADESAO COMPULSORIA - INCLUSAO           *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTNV-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTNV-ENTRADA.
           03 GFCTNV-LL                            PIC S9(004) COMP.
           03 GFCTNV-ZZ                            PIC S9(004) COMP.
           03 GFCTNV-TRANSACAO                     PIC  X(009).
           03 GFCTNV-FUNCAO                        PIC  X(008).
           03 GFCTNV-DADOS-RST.
              05 GFCTNV-FUNC-BDSCO                 PIC  X(007).
              05 GFCTNV-TERMINAL                   PIC  X(008).
              05 GFCTNV-FIM                        PIC  X(001).
              05 GFCTNV-FILLER                     PIC  X(608).
           03 GFCTNV-FIXO.
              05 GFCTNV-CAGENCIA-EXCEC             PIC  9(005).
              05 GFCTNV-CCONTA-EXCEC               PIC  9(013).
              05 GFCTNV-RCONTA-EXCEC               PIC  X(050).
              05 GFCTNV-CPACOTE                    PIC  9(005).
              05 GFCTNV-CAGPTO                     PIC  9(003).
              05 GFCTNV-DINI-ADESAO                PIC  X(010).
              05 GFCTNV-SEQUENCIA                  PIC  9(009).
              05 GFCTNV-DATA-INICIO-EXCEC          PIC  X(010).
              05 GFCTNV-DATA-FIM-EXCEC             PIC  X(010).
