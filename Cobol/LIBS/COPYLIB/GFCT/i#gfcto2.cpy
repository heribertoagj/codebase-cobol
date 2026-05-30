      ******************************************************************
      * NOME DA INC - I#GFCTO2                                         *
      * DESCRICAO   - COMBO DE PACOTE - TABELA GFCTB001                *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 24/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTO2-SAIDA.
           03  GFCTO2-LL                            PIC S9(004) COMP.
           03  GFCTO2-ZZ                            PIC S9(004) COMP.
           03  GFCTO2-TRANSACAO                     PIC  X(009).
           03  GFCTO2-FUNCAO                        PIC  X(008).
           03  GFCTO2-FILTRO                        PIC  9(005).
           03  GFCTO2-QTDE-OCOR                     PIC  9(003).
           03  GFCTO2-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTO2-FUNC-BDSCO                    PIC  X(007).
           03  GFCTO2-PONTEIRO.
               05  GFCTO2-PONTEIRO-PACOTE           PIC  9(005).
           03  GFCTO2-ERRO                          PIC  9(001).
           03  GFCTO2-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTO2-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTO2-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTO2-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(032).
           03  GFCTO2-DADOS.
               05  FILLER                           OCCURS 8.
                   07  GFCTO2-COD-PACOTE            PIC  9(005).
                   07  GFCTO2-DESC-PACOTE           PIC  X(100).
