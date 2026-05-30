***************************** Top of Data ******************************
      ******************************************************************
      * NOME DA INC - I#GFCTMU                                         *
      * DESCRICAO   - COMBO TIPO AGRUPAMENTO                           *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 07/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTMU-SAIDA.
           03  GFCTMU-LL                            PIC S9(004) COMP.
           03  GFCTMU-ZZ                            PIC S9(004) COMP.
           03  GFCTMU-TRANSACAO                     PIC  X(009).
           03  GFCTMU-FUNCAO                        PIC  X(008).
           03  GFCTMU-FILTRO                        PIC  9(003).
           03  GFCTMU-QTDE-OCOR                     PIC  9(003).
           03  GFCTMU-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTMU-FUNC-BDSCO                    PIC  X(007).
           03  GFCTMU-TERMINAL                      PIC  X(008).
           03  GFCTMU-PONTEIRO                      PIC  9(003).
           03  GFCTMU-ERRO                          PIC  9(001).
           03  GFCTMU-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTMU-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTMU-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTMU-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(208).
           03  GFCTMU-DADOS.
               05  FILLER                           OCCURS 20.
                   07  GFCTMU-TP-AGRUP              PIC  9(003).
                   07  GFCTMU-DESC-AGRUP            PIC  X(030).
