      ******************************************************************
      * NOME DA INC - I#GFCTOE                                         *
      * DESCRICAO   - COMBO DE TIPO DE AGRUPAMENTO                     *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 29/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTOE-SAIDA.
           03  GFCTOE-LL                            PIC S9(004) COMP.
           03  GFCTOE-ZZ                            PIC S9(004) COMP.
           03  GFCTOE-TRANSACAO                     PIC  X(009).
           03  GFCTOE-FUNCAO                        PIC  X(008).
           03  GFCTOE-FILTRO                        PIC  9(005).
           03  GFCTOE-QTDE-OCOR                     PIC  9(003).
           03  GFCTOE-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTOE-FUNC-BDSCO                    PIC  X(007).
           03  GFCTOE-PONTEIRO.
               05  GFCTOE-PONTEIRO-AGRUP            PIC  9(003).
           03  GFCTOE-ERRO                          PIC  9(001).
           03  GFCTOE-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTOE-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTOE-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTOE-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(049).
           03  GFCTOE-DADOS.
               05  FILLER                           OCCURS 25.
                   07  GFCTOE-COD-AGRUP             PIC  9(003).
                   07  GFCTOE-DESC-AGRUP            PIC  X(030).
