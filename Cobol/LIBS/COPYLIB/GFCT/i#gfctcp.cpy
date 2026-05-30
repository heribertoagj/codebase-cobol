      ******************************************************************
      * NOME DA INC - I#GFCTCP                                         *
      * DESCRICAO   - COMBO DE TIPO DE OPERACAO                        *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 09.06.2005                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41956693        *
      ******************************************************************
      *
       01  GFCTCP-SAIDA.
           03  GFCTCP-LL                            PIC S9(004) COMP.
           03  GFCTCP-ZZ                            PIC S9(004) COMP.
           03  GFCTCP-TRANSACAO                     PIC  X(009).
           03  GFCTCP-FUNCAO                        PIC  X(008).
           03  GFCTCP-FILTRO                        PIC  9(003).
           03  GFCTCP-QTDE-OCOR                     PIC  9(003).
           03  GFCTCP-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTCP-FUNC-BDSCO                    PIC  X(007).
           03  GFCTCP-TERMINAL                      PIC  X(008).
           03  GFCTCP-PONTEIRO                      PIC  9(003).
           03  GFCTCP-ERRO                          PIC  9(001).
           03  GFCTCP-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTCP-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTCP-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTCP-FIM                           PIC  X(001).
           03  GFCTCP-COD-EXCL-LOGCA                PIC  X(001).
      *        'T' - TODOS (ATIVOS E CANCELADOS)
      *        'A' - ATIVOS
      *        'C' - CANCELADOS
           03  GFCTCP-COD-UTILIZACAO                PIC  9(001).
      *        0 - NENHUM
      *        1 - COM REGISTRO
      *        2 - SEM REGISTRO
      *        3 - TODOS
           03  GFCTCP-COD-FUNCIONALIDADE            PIC  9(001).
      *        0 - NENHUM
      *        1 - ESTORNO
      *        2 - FLEXIBILIZACAO
      *        3 - CANCELAMENTO
      *        4 - TODOS
      *        5 - ESTORNO E FLEXIBILIZACAO
           03  FILLER                               PIC  X(025).
           03  GFCTCP-DADOS.
               05  FILLER                           OCCURS 8.
                   07  GFCTCP-COD-OPERACAO          PIC  9(003).
                   07  GFCTCP-DESC-OPERACAO         PIC  X(100).
                   07  GFCTCP-COD-UTILIZ            PIC  9(001).
                   07  GFCTCP-COD-FUNCIONALID       PIC  9(001).
