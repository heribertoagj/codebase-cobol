      ******************************************************************
      * NOME DA INC - GFCTWAGJ                                         *
      * DESCRICAO   - COMBO TIPO AGRUPAMENTO PERSONALIZADA             *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 03/2009                                          *
      * RESPONSAVEL - WAGNER SILVA    - PROCWORK - TEL 4506-2345       *
      ******************************************************************
      *
       01  WAGJ-ENTRADA.
           03  WAGJ-LL                              PIC S9(004) COMP.
           03  WAGJ-ZZ                              PIC S9(004) COMP.
           03  WAGJ-TRANSACAO                       PIC  X(009).
           03  WAGJ-FUNCAO                          PIC  X(008).
           03  WAGJ-OPCAO                           PIC  9(001).
      *        1=CANCELAMENTO / 2=DEFERIMENTO / 3/CONSULTA
           03  WAGJ-DADOS-RST.
               05  WAGJ-FILTRO                      PIC  9(003).
               05  WAGJ-QTDE-OCOR                   PIC  9(003).
               05  WAGJ-QTDE-TOT-REG                PIC  9(006).
               05  WAGJ-FUNC-BDSCO                  PIC  X(007).
               05  WAGJ-TERMINAL                    PIC  X(008).
               05  WAGJ-PONTEIRO                    PIC  9(003).
               05  WAGJ-FIM                         PIC  X(001).
               05  FILLER                           PIC  X(047).
