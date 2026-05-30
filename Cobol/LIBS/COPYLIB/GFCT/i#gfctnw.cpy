      *****************************************************************
      * NOME DA INC - I#GFCTNW                                        *
      * DESCRICAO   - EXCECAO DE ADESAO COMPULSORIA - INCLUSAO        *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTNW-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTNW-SAIDA.
           03 GFCTNW-LL                            PIC S9(004) COMP.
           03 GFCTNW-ZZ                            PIC S9(004) COMP.
           03 GFCTNW-TRANSACAO                     PIC  X(009).
           03 GFCTNW-FUNCAO                        PIC  X(008).
           03 GFCTNW-DADOS-RST.
              05 GFCTNW-FUNC-BDSCO                 PIC  X(007).
              05 GFCTNW-TERMINAL                   PIC  X(008).
              05 GFCTNW-FIM                        PIC  X(001).
           03 GFCTNW-ERRO                          PIC  9(001).
           03 GFCTNW-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTNW-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTNW-DESC-MSG-ERRO                 PIC  X(079).
           03 GFCTNW-FILLER                        PIC  X(518).
           03 GFCTNW-FIXO.
              05 GFCTNW-CAGENCIA-EXCEC             PIC  9(005).
              05 GFCTNW-CCONTA-EXCEC               PIC  9(013).
              05 GFCTNW-RCONTA-EXCEC               PIC  X(050).
              05 GFCTNW-CPACOTE                    PIC  9(005).
              05 GFCTNW-CAGPTO                     PIC  9(003).
              05 GFCTNW-DINI-ADESAO                PIC  X(010).
              05 GFCTNW-SEQUENCIA                  PIC  9(009).
              05 GFCTNW-DATA-INICIO-EXCEC          PIC  X(010).
              05 GFCTNW-DATA-FIM-EXCEC             PIC  X(010).
