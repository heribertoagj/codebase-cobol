      ******************************************************************
      * NOME DA INC - I#GFCTOL                                         *
      * DESCRICAO   - COMBO TIPO AGRUPAMENTO - FLEXIBILIZACAO          *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 01/04/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTOL-SAIDA.
           03  GFCTOL-LL                            PIC S9(004) COMP.
           03  GFCTOL-ZZ                            PIC S9(004) COMP.
           03  GFCTOL-TRANSACAO                     PIC  X(009).
           03  GFCTOL-FUNCAO                        PIC  X(008).
           03  GFCTOL-FILTRO                        PIC  9(003).
           03  GFCTOL-QTDE-OCOR                     PIC  9(003).
           03  GFCTOL-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTOL-FUNC-BDSCO                    PIC  X(007).
           03  GFCTOL-PONTEIRO                      PIC  9(003).
           03  GFCTOL-ERRO                          PIC  9(001).
           03  GFCTOL-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTOL-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTOL-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTOL-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(196).
           03  GFCTOL-DADOS.
               05  FILLER                           OCCURS 20.
                   07  GFCTOL-TP-AGRUP              PIC  9(003).
                   07  GFCTOL-DESC-AGRUP            PIC  X(030).
PROC               07  GFCTOL-PERSONALIZ            PIC  X(001).
