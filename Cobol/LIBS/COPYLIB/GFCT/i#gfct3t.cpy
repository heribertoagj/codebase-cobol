      ******************************************************************
      * NOME DA INC - I#GFCT3T                                         *
      * DESCRICAO   - MOVIMENTO ONLINE - MOTIVO DO CANCELAMENTO        *
      *               COMBO                                            *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 16.12.2004                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693        *
      ******************************************************************
      *
       01  GFCT3T-ENTRADA.
           03  GFCT3T-LL                            PIC S9(004) COMP.
           03  GFCT3T-ZZ                            PIC S9(004) COMP.
           03  GFCT3T-TRANSACAO                     PIC  X(009).
           03  GFCT3T-FUNCAO                        PIC  X(008).
           03  GFCT3T-DADOS-RST.
               05  GFCT3T-FILTRO                    PIC  9(003).
               05  GFCT3T-QTDE-OCOR                 PIC  9(003).
               05  GFCT3T-QTDE-TOT-REG              PIC  9(006).
               05  GFCT3T-FUNC-BDSCO                PIC  X(007).
               05  GFCT3T-TERMINAL                  PIC  X(008).
               05  GFCT3T-PONTEIRO                  PIC  9(003).
               05  GFCT3T-FIM                       PIC  X(001).
               05  GFCT3T-COD-EXCL-LOGCA            PIC  X(001).
      *            'T' - TODOS (ATIVOS E CANCELADOS)
      *            'A' - ATIVOS
      *            'C' - CANCELADOS
               05  FILLER                           PIC  X(047).
