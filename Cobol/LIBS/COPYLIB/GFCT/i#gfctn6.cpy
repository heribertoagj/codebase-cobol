      ******************************************************************
      * NOME DA INC - I#GFCTN6                                         *
      * DESCRICAO   - COMBO DE AGRUPAMENTO - TABELA GFCTB001           *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 10/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTN6-SAIDA.
           03  GFCTN6-LL                            PIC S9(004) COMP.
           03  GFCTN6-ZZ                            PIC S9(004) COMP.
           03  GFCTN6-TRANSACAO                     PIC  X(009).
           03  GFCTN6-FUNCAO                        PIC  X(008).
           03  GFCTN6-FILTRO.
               05  GFCTN6-DATA-REFER                PIC  X(010).
           03  GFCTN6-QTDE-OCOR                     PIC  9(003).
           03  GFCTN6-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTN6-FUNC-BDSCO                    PIC  X(007).
           03  GFCTN6-PONTEIRO.
               05  GFCTN6-PONTEIRO-AGRUP            PIC  9(003).
               05  GFCTN6-PONTEIRO-DATA             PIC  X(010).
           03  GFCTN6-ERRO                          PIC  9(001).
           03  GFCTN6-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTN6-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTN6-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTN6-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(199).
           03  GFCTN6-DADOS.
               05  FILLER                           OCCURS 20.
                   07  GFCTN6-COD-AGRUPAMENTO       PIC  9(003).
                   07  GFCTN6-DESC-AGRUPAMENTO      PIC  X(030).
