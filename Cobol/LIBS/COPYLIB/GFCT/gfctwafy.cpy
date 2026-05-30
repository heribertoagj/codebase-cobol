      ******************************************************************
      * NOME DA INC - GFCTWAFY                                         *
      * DESCRICAO   - COMBO TIPO AGRUPAMENTO                           *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 16/02/2009                                       *
      * RESPONSAVEL - WAGNER SILVA    - PROCWORK - TEL 4506-2345       *
      ******************************************************************
      *
       01  WAFY-SAIDA.
           03  WAFY-LL                              PIC S9(004) COMP.
           03  WAFY-ZZ                              PIC S9(004) COMP.
           03  WAFY-TRANSACAO                       PIC  X(009).
           03  WAFY-FUNCAO                          PIC  X(008).
           03  WAFY-FILTRO                          PIC  9(003).
           03  WAFY-QTDE-OCOR                       PIC  9(003).
           03  WAFY-QTDE-TOT-REG                    PIC  9(006).
           03  WAFY-FUNC-BDSCO                      PIC  X(007).
           03  WAFY-TERMINAL                        PIC  X(008).
           03  WAFY-PONTEIRO                        PIC  9(003).
           03  WAFY-ERRO                            PIC  9(001).
           03  WAFY-COD-MSG-ERRO                    PIC  9(007).
           03  WAFY-COD-SQL-ERRO                    PIC  9(003).
           03  WAFY-DESC-MSG-ERRO                   PIC  X(079).
           03  WAFY-FIM                             PIC  X(001).
           03  FILLER                               PIC  X(208).
           03  WAFY-DADOS.
               05  FILLER                           OCCURS 20.
                   07  WAFY-TP-AGRUP                PIC  9(003).
                   07  WAFY-DESC-AGRUP              PIC  X(030).
