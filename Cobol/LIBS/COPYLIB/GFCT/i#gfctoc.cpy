      ******************************************************************
      * NOME DA INC - I#GFCTOC                                         *
      * DESCRICAO   - COMBO DE PACOTES - TABELA GFCTB048               *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 29/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTOC-SAIDA.
           03  GFCTOC-LL                            PIC S9(004) COMP.
           03  GFCTOC-ZZ                            PIC S9(004) COMP.
           03  GFCTOC-TRANSACAO                     PIC  X(009).
           03  GFCTOC-FUNCAO                        PIC  X(008).
           03  GFCTOC-FILTRO                        PIC  9(005).
           03  GFCTOC-QTDE-OCOR                     PIC  9(003).
           03  GFCTOC-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTOC-FUNC-BDSCO                    PIC  X(007).
           03  GFCTOC-PONTEIRO.
               05  GFCTOC-PONTEIRO-PACOTE           PIC  9(005).
           03  GFCTOC-ERRO                          PIC  9(001).
           03  GFCTOC-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTOC-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTOC-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTOC-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(032).
           03  GFCTOC-DADOS.
               05  FILLER                           OCCURS 8.
                   07  GFCTOC-COD-PACOTE            PIC  9(005).
                   07  GFCTOC-DESC-PACOTE           PIC  X(100).
