      ******************************************************************
      * NOME DA INC - I#GFCTCO                                         *
      * DESCRICAO   - COMBO DE TIPO DE OPERACAO                        *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 09.06.2005                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693        *
      ******************************************************************
      *
       01  GFCTCO-ENTRADA.
           03  GFCTCO-LL                            PIC S9(004) COMP.
           03  GFCTCO-ZZ                            PIC S9(004) COMP.
           03  GFCTCO-TRANSACAO                     PIC  X(009).
           03  GFCTCO-FUNCAO                        PIC  X(008).
           03  GFCTCO-DADOS-RST.
               05  GFCTCO-FILTRO                    PIC  9(003).
               05  GFCTCO-QTDE-OCOR                 PIC  9(003).
               05  GFCTCO-QTDE-TOT-REG              PIC  9(006).
               05  GFCTCO-FUNC-BDSCO                PIC  X(007).
               05  GFCTCO-TERMINAL                  PIC  X(008).
               05  GFCTCO-PONTEIRO                  PIC  9(003).
               05  GFCTCO-FIM                       PIC  X(001).
               05  GFCTCO-COD-EXCL-LOGCA            PIC  X(001).
      *            'T' - TODOS (ATIVOS E CANCELADOS)
      *            'A' - ATIVOS
      *            'C' - CANCELADOS
               05  GFCTCO-COD-UTILIZACAO            PIC  9(001).
      *             0 - NENHUM
      *             1 - COM REGISTRO
      *             2 - SEM REGISTRO
      *             3 - TODOS
               05  GFCTCO-COD-FUNCIONALIDADE        PIC  9(001).
      *             0 - NENHUM
      *             1 - ESTORNO
      *             2 - FLEXIBILIZACAO
      *             3 - CANCELAMENTO
      *             4 - TODOS
      *             5 - ESTORNO E FLEXIBILIZACAO
               05  FILLER                           PIC  X(045).
