      *****************************************************************
      * NOME DA INC - I#GFCTP7                                        *
      * DESCRICAO   - SUSPENSAO DE TARIFA - INCLUSAO                  *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTP7-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTP7-SAIDA.
           03 GFCTP7-LL                            PIC S9(004) COMP.
           03 GFCTP7-ZZ                            PIC S9(004) COMP.
           03 GFCTP7-TRANSACAO                     PIC  X(009).
           03 GFCTP7-FUNCAO                        PIC  X(008).
           03 GFCTP7-DADOS-RST.
              05 GFCTP7-FUNC-BDSCO                 PIC  X(007).
              05 GFCTP7-ERRO                       PIC  9(001).
              05 GFCTP7-COD-MSG-ERRO               PIC  9(007).
              05 GFCTP7-COD-SQL-ERRO               PIC  9(003).
              05 GFCTP7-DESC-MSG-ERRO              PIC  X(079).
              05 GFCTP7-FIM                        PIC  X(001).
              05 GFCTP7-FILLER                     PIC  X(574).
           03 GFCTP7-FIXO.
              05 GFCTP7-CTARIFA                    PIC  9(005).
              05 GFCTP7-CAGPTO                     PIC  9(003).
              05 GFCTP7-TOTAL                      PIC  X(001).
              05 GFCTP7-CGC-CPF                    PIC  9(009).
              05 GFCTP7-FILIAL                     PIC  9(005).
              05 GFCTP7-CONTROLE                   PIC  X(002).
              05 GFCTP7-CAGENCIA                   PIC  9(005).
              05 GFCTP7-CPAB                       PIC  9(005).
              05 GFCTP7-CMUNIC                     PIC  9(005).
              05 GFCTP7-CUF                        PIC  X(002).
              05 GFCTP7-DATA-INICIO                PIC  X(010).
              05 GFCTP7-DATA-FIM                   PIC  X(010).
              05 GFCTP7-JUST                       PIC  X(255).
