      ******************************************************************
      * NOME DA INC - GFCTWAFX                                         *
      * DESCRICAO   - COMBO TIPO AGRUPAMENTO                           *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 16/02/2009                                       *
      * RESPONSAVEL - WAGNER SILVA    - PROCWORK - TEL 4506-2345       *
      ******************************************************************
      *
       01  WAFX-ENTRADA.
           03  WAFX-LL                              PIC S9(004) COMP.
           03  WAFX-ZZ                              PIC S9(004) COMP.
           03  WAFX-TRANSACAO                       PIC  X(009).
           03  WAFX-FUNCAO                          PIC  X(008).
           03  WAFX-DADOS-RST.
               05  WAFX-FILTRO                      PIC  9(003).
               05  WAFX-QTDE-OCOR                   PIC  9(003).
               05  WAFX-QTDE-TOT-REG                PIC  9(006).
               05  WAFX-FUNC-BDSCO                  PIC  X(007).
               05  WAFX-TERMINAL                    PIC  X(008).
               05  WAFX-PONTEIRO                    PIC  9(003).
               05  WAFX-FIM                         PIC  X(001).
               05  FILLER                           PIC  X(048).
