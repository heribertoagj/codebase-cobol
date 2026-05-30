      ******************************************************************
      * NOME DA INC - I#GFCT3U                                         *
      * DESCRICAO   - MOVIMENTO ONLINE - MOTIVO DO CANCELAMENTO        *
      *               COMBO                                            *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 16.12.2004                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693        *
      ******************************************************************
      *
       01  GFCT3U-SAIDA.
           03  GFCT3U-LL                            PIC S9(004) COMP.
           03  GFCT3U-ZZ                            PIC S9(004) COMP.
           03  GFCT3U-TRANSACAO                     PIC  X(009).
           03  GFCT3U-FUNCAO                        PIC  X(008).
           03  GFCT3U-FILTRO                        PIC  9(003).
           03  GFCT3U-QTDE-OCOR                     PIC  9(003).
           03  GFCT3U-QTDE-TOT-REG                  PIC  9(006).
           03  GFCT3U-FUNC-BDSCO                    PIC  X(007).
           03  GFCT3U-TERMINAL                      PIC  X(008).
           03  GFCT3U-PONTEIRO                      PIC  9(003).
           03  GFCT3U-ERRO                          PIC  9(001).
           03  GFCT3U-COD-MSG-ERRO                  PIC  9(007).
           03  GFCT3U-COD-SQL-ERRO                  PIC  9(003).
           03  GFCT3U-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCT3U-FIM                           PIC  X(001).
           03  GFCT3U-COD-EXCL-LOGCA                PIC  X(001).
      *      'T' - TODOS (ATIVOS E CANCELADOS)
      *      'A' - ATIVOS
      *      'C' - CANCELADOS
           03  FILLER                               PIC  X(003).
           03  GFCT3U-DADOS.
               05  FILLER                           OCCURS 27.
                   07  GFCT3U-COD-MOTIVO            PIC  9(002).
                   07  GFCT3U-DESC-MOTIVO           PIC  X(030).
