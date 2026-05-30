      ******************************************************************
      * NOME DA INC - GFCTWAGK                                         *
      * DESCRICAO   - COMBO TIPO AGRUPAMENTO                           *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 03/2009                                          *
      * RESPONSAVEL - WAGNER SILVA    - PROCWORK - TEL 4506-2345       *
      ******************************************************************
      *
       01  WAGK-SAIDA.
           03  WAGK-LL                              PIC S9(004) COMP.
           03  WAGK-ZZ                              PIC S9(004) COMP.
           03  WAGK-TRANSACAO                       PIC  X(009).
           03  WAGK-FUNCAO                          PIC  X(008).
           03  WAGK-FILTRO                          PIC  9(003).
           03  WAGK-QTDE-OCOR                       PIC  9(003).
           03  WAGK-QTDE-TOT-REG                    PIC  9(006).
           03  WAGK-FUNC-BDSCO                      PIC  X(007).
           03  WAGK-TERMINAL                        PIC  X(008).
           03  WAGK-PONTEIRO                        PIC  9(003).
           03  WAGK-ERRO                            PIC  9(001).
           03  WAGK-COD-MSG-ERRO                    PIC  9(007).
           03  WAGK-COD-SQL-ERRO                    PIC  9(003).
           03  WAGK-DESC-MSG-ERRO                   PIC  X(079).
           03  WAGK-OPCAO                           PIC  9(001).
      *        1=CANCELAMENTO / 2=DEFERIMENTO / 3/CONSULTA
           03  WAGK-FIM                             PIC  X(001).
           03  FILLER                               PIC  X(207).
           03  WAGK-DADOS.
               05  FILLER                           OCCURS 20.
                   07  WAGK-TP-AGRUP                PIC  9(003).
                   07  WAGK-DESC-AGRUP              PIC  X(030).

